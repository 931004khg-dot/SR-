;; SR.lsp - 객체 측정

(defun C:SR (/ acad_doc old_error g_sr_selection_set 
               g_saved_view_center g_saved_paper_view_center
               g_viewport_width g_viewport_height g_paper_viewport_width g_paper_viewport_height
               g_temp_measurement_object_list
               g_original_vp_layer_states g_layer_separation_active
               ;; g_restore_view_enabled 는 전역 변수로 관리하므로 C:SR 지역 변수 목록에서 제거
               mode user_input target_vp_obj sel_obj_ename sel_obj_type)
  (vl-load-com)
  (setq acad_doc (vla-get-activedocument (vlax-get-acad-object)))

  ;; --- 선택집합을 통한 객체 강조를 위한 전역 변수 ---
  (setq g_sr_selection_set nil)
  ;; --- 저장된 화면 상태를 위한 전역 변수 ---
  (setq g_saved_view_center nil)        ; 모형공간 측정 완료 시 저장 (다음 측정 시 복원용)
  (setq g_saved_text_center nil)        ; 모형공간 텍스트 선택 시 저장 (다음 텍스트 선택 시 복원용)
  (setq g_saved_paper_view_center nil)  ; 배치공간 측정 완료 시 저장 (다음 측정 시 복원용)
  (setq g_saved_paper_text_center nil)  ; 배치공간 텍스트 선택 시 저장 (다음 텍스트 선택 시 복원용)
  ;; --- 뷰포트 기준 크기를 위한 전역 변수 ---
  (setq g_viewport_width nil g_viewport_height nil)
  (setq g_paper_viewport_width nil g_paper_viewport_height nil)
  ;; --- 임시 측정 객체를 위한 전역 변수 ---
  (setq g_temp_measurement_object_list nil)
  ;; --- 레이어 분리 관련 전역 변수 ---
  (setq g_original_vp_layer_states nil)
  (setq g_layer_separation_active nil)
  ;; --- 화면 복원 여부 전역 변수 ---
  (setq g_restore_view_enabled nil)


  ;; --- ESC 또는 오류 발생 시 실행될 안전 장치(오류 핸들러) ---
  (defun sr_error_handler (msg)
  ;; 선택집합 해제
  (if g_sr_selection_set
    (progn
      (sssetfirst nil nil) ; 하이라이트만 안전하게 제거
      (setq g_sr_selection_set nil) ; 전역 변수 초기화
    )
  )
    ;; 임시 측정 객체들 삭제
    (if g_temp_measurement_object_list
      (progn
        (foreach temp_obj g_temp_measurement_object_list
          (delete-temp-object temp_obj)
        )
        (setq g_temp_measurement_object_list nil)
      )
    )
    ;; 레이어 상태 복원
    (if g_layer_separation_active
      (progn
        (prompt "\n오류 발생! 레이어 상태를 복원합니다...")
        (restore-layer-states)
      )
    )
    (if old_error (setq *error* old_error)) ; 기존 오류 핸들러 복원
    (if (not (wcmatch (strcase msg) "*QUIT,*CANCEL*"))
        (princ (strcat "\n오류: " msg)))
    (princ)
  )

  ;; --- 레이어 분리 관련 함수들 ---
  (defun find-viewport-from-polyline (pline_ename / pline_data linked_entity_name linked_entity_data viewport_ename)
    (setq pline_data (entget pline_ename))
    (setq viewport_ename nil)
    (foreach data pline_data
      (if (= (car data) 330)
        (progn
          (setq linked_entity_name (cdr data))
          (setq linked_entity_data (entget linked_entity_name))
          (if (and linked_entity_data (= (cdr (assoc 0 linked_entity_data)) "VIEWPORT"))
            (setq viewport_ename linked_entity_name)))))
    (if viewport_ename (vlax-ename->vla-object viewport_ename) nil))

  (defun is-mvo-polyline (pline_ename / result)
    (setq result (find-viewport-from-polyline pline_ename))
    (if result t nil))

  (defun remove-duplicates (lst / result)
    (setq result '())
    (foreach item lst
      (if (not (member item result))
        (setq result (cons item result))))
    (reverse result))

  (defun layers-to-string (layer_list / result)
    (setq result "")
    (foreach layer layer_list
      (setq result (strcat result "'" layer "' ")))
    result)

  (defun extract-base-layer-name (full_layer_name / pipe_pos)
    (setq pipe_pos (vl-string-search "|" full_layer_name))
    (if pipe_pos
      (substr full_layer_name (+ pipe_pos 2))
      full_layer_name
    )
  )

  (defun is-layer-match (target_layer current_layer / base_target base_current)
    (setq base_target (extract-base-layer-name target_layer)
          base_current (extract-base-layer-name current_layer))
    (or (= target_layer current_layer)
        (= base_target base_current)
        (and (vl-string-search "|" current_layer)
             (= base_target base_current))
        (and (vl-string-search "|" target_layer)
             (= base_target base_current))
    )
  )

  ;; 블록이 외부참조인지 확인하는 함수 (vla 사용하지 않음)
  (defun is-xref-block (blk_name / blk_table blk_record)
    (setq blk_table (tblsearch "BLOCK" blk_name))
    (if blk_table
      (= 4 (logand 4 (cdr (assoc 70 blk_table))))  ; 외부참조 플래그 확인
      nil
    )
  )

  ;; 블록 내부 객체들의 레이어 수집 함수
  (defun get-block-layers (blk_name / blk_def ent_list layer_list)
    (setq blk_def (tblsearch "BLOCK" blk_name)
          ent_list '()
          layer_list '())
    (if blk_def
      (progn
        ;; 블록 정의 내 모든 객체 찾기
        (setq ent_name (cdr (assoc -2 blk_def)))  ; 블록 정의 첫 객체
        (while ent_name
          (setq ent_data (entget ent_name))
          (if ent_data
            (progn
              (setq layer_name (cdr (assoc 8 ent_data)))
              (if (and layer_name (not (member layer_name layer_list)))
                (setq layer_list (cons layer_name layer_list))
              )
              (setq ent_name (entnext ent_name))
            )
            (setq ent_name nil)
          )
        )
      )
    )
    layer_list
  )

  ;; 임시 측정 객체용 레이어가 존재하는지 확인하고 없으면 생성하는 함수
  (defun ensure-temp-measurement-layer (/ layers layer_obj temp_layer_name)
    (setq temp_layer_name "SR_TEMP_MEASUREMENT")
    (setq layers (vla-get-layers acad_doc))
    
    ;; 레이어가 이미 존재하는지 확인
    (if (vl-catch-all-error-p 
          (vl-catch-all-apply 'vla-item (list layers temp_layer_name)))
      ;; 레이어가 없으면 생성
      (progn
        (setq layer_obj (vla-add layers temp_layer_name))
        (vla-put-color layer_obj 2)  ; 노란색
        (prompt (strcat "\n>> 임시 측정용 레이어 '" temp_layer_name "' 생성됨"))
      )
      ;; 레이어가 이미 있으면 가져오기
      (setq layer_obj (vla-item layers temp_layer_name))
    )
    
    ;; 레이어 켜기 (레이어 분리 상태에서도 항상 보이도록)
    (vla-put-layeron layer_obj :vlax-true)
    
    temp_layer_name
  )

  (defun perform-layer-separation (current_mode / sel_set target_layer_names layers layer_obj layer_name
                                      paperspace_layers ss i ent prompt_list_str obj_ename obj_data obj_type
                                      xref_block_names block_ref_names final_target_layers blk_name current_layer_name
                                      block_layers temp_layer_name)
    (prompt "\n=== 레이어 분리 모드 ===")
    (prompt "\n표시할 레이어의 객체들을 선택하세요 (Enter로 완료): ")
    (setq sel_set (ssget))
    
    (if sel_set
      (progn
        (setq target_layer_names '() 
              xref_block_names '() 
              block_ref_names '()
              i 0)
        
        ;; 선택된 객체들 분석
        (repeat (sslength sel_set)
          (setq obj_ename (ssname sel_set i)
                obj_data (entget obj_ename)
                obj_type (cdr (assoc 0 obj_data))
                layer_name (cdr (assoc 8 obj_data)))
          
          (cond
            ;; 블록 참조인 경우
            ((= obj_type "INSERT")
             (setq blk_name (cdr (assoc 2 obj_data)))
             (if (is-xref-block blk_name)
               ;; 외부참조인 경우
               (if (not (member blk_name xref_block_names))
                 (setq xref_block_names (cons blk_name xref_block_names))
               )
               ;; 일반 블록인 경우
               (if (not (member blk_name block_ref_names))
                 (setq block_ref_names (cons blk_name block_ref_names))
               )
             ))
            ;; 다른 객체인 경우 레이어만 추가
            (t
             (if (and layer_name (not (member layer_name target_layer_names)))
               (setq target_layer_names (cons layer_name target_layer_names))
             ))
          )
          (setq i (1+ i))
        )
        
        (setq final_target_layers (remove-duplicates target_layer_names))
        
        ;; 외부참조 레이어 처리
        (if xref_block_names
          (progn
            (setq layers (vla-get-layers acad_doc))
            (vlax-for layer_obj layers
              (setq current_layer_name (vla-get-name layer_obj))
              (foreach name xref_block_names
                (if (wcmatch (strcase current_layer_name) (strcat (strcase name) "|*"))
                  (if (not (member current_layer_name final_target_layers))
                    (setq final_target_layers (cons current_layer_name final_target_layers))
                  )
                )
              )
            )
          )
        )
        
        ;; 일반 블록 레이어 처리
        (if block_ref_names
          (foreach blk_name block_ref_names
            (setq block_layers (get-block-layers blk_name))
            (foreach blk_layer block_layers
              (if (not (member blk_layer final_target_layers))
                (setq final_target_layers (cons blk_layer final_target_layers))
              )
            )
          )
        )
        
        ;; ★★★ 임시 측정 객체용 레이어를 최종 표시 레이어 목록에 추가 ★★★
        (setq temp_layer_name (ensure-temp-measurement-layer))
        (if (not (member temp_layer_name final_target_layers))
          (setq final_target_layers (cons temp_layer_name final_target_layers))
        )
        
        ;; 프롬프트 메시지 생성
        (setq prompt_list_str "")
        (foreach name (reverse xref_block_names)
          (setq prompt_list_str (strcat prompt_list_str "외부참조'" name "' ")))
        (foreach name (reverse block_ref_names)
          (setq prompt_list_str (strcat prompt_list_str "블록참조'" name "' ")))
        (foreach name (reverse target_layer_names)
          (setq prompt_list_str (strcat prompt_list_str "'" name "' ")))

        (prompt (strcat "\n표시될 레이어 및 객체: " prompt_list_str))
        (prompt (strcat "\n총 " (itoa (length final_target_layers)) "개 레이어가 표시됩니다."))

        ;; 원본 레이어 상태 저장
        (setq g_original_vp_layer_states nil)
        (setq layers (vla-get-layers acad_doc))

        (vlax-for layer_obj layers
          (setq g_original_vp_layer_states
            (cons (cons (vla-get-name layer_obj) (vla-get-layeron layer_obj))
                  g_original_vp_layer_states)))

        ;; 배치공간 레이어 보존
        (setq paperspace_layers '("0"))
        (if (= current_mode "viewport")
          (if (setq ss (ssget "_X" (list '(0 . "*") (cons 410 (getvar "CTAB")))))
            (progn
              (setq i 0)
              (repeat (sslength ss)
                (setq ent (ssname ss i) layer_name (cdr (assoc 8 (entget ent))))
                (if (not (member layer_name paperspace_layers))
                  (setq paperspace_layers (cons layer_name paperspace_layers)))
                (setq i (1+ i))))))
        
        ;; 레이어 켜기/끄기 적용
        (vlax-for layer_obj layers
          (setq layer_name (vla-get-name layer_obj))
          (cond
            ((member layer_name final_target_layers)
             (vla-put-layeron layer_obj :vlax-true))
            ((member layer_name paperspace_layers)
             nil)  ; 배치공간 레이어는 그대로 두기
            (t (vla-put-layeron layer_obj :vlax-false))))
        
        (setq g_layer_separation_active t)
        (prompt "\n>> 레이어 분리 완료: 선택된 객체 및 레이어만 표시됩니다.")
        (prompt "\n>> 임시 측정 객체는 레이어 분리와 관계없이 항상 표시됩니다.")
        t
      )
      (progn
        (prompt "\n>> 레이어 분리가 취소되었습니다.")
        nil
      )
    )
  )

  (defun restore-layer-states (/ layer_obj layers)
    (if (and g_original_vp_layer_states g_layer_separation_active)
      (progn
        (setq layers (vla-get-layers acad_doc))
        (foreach state g_original_vp_layer_states
          (if (not (vl-catch-all-error-p 
                     (vl-catch-all-apply 'vla-item (list layers (car state)))))
            (progn
              (setq layer_obj (vla-item layers (car state)))
              (vla-put-layeron layer_obj (cdr state))
            )
          )
        )
        (setq g_original_vp_layer_states nil
              g_layer_separation_active nil)
        (prompt "\n>> 레이어 상태가 복원되었습니다.")
      )
    )
  )

  (defun get-measurement-data (obj_vla measurement_mode / ent_data ent_type value prefix suffix)
    (setq ent_type (vla-get-objectname obj_vla))
    (setq value nil prefix nil suffix nil)
    (cond
      ((= ent_type "AcDbPolyline")
       (setq ent_data (entget (vlax-vla-object->ename obj_vla)))
       (if (and ent_data (= 1 (logand 1 (cdr (assoc 70 ent_data)))))
         (if (= measurement_mode "LENGTH")
           (progn (setq prefix "L=") (setq value (vla-get-length obj_vla)) (setq suffix "m"))
           (progn (setq prefix "A=") (setq value (vla-get-area obj_vla)) (setq suffix "㎡"))
         )
         (progn (setq prefix "L=") (setq value (vla-get-length obj_vla)) (setq suffix "m"))
       ))
      ((= ent_type "AcDbLine")
       (setq prefix "L=") (setq value (vla-get-length obj_vla)) (setq suffix "m"))
      ((= ent_type "AcDbArc")
       (setq prefix "L=") (setq value (vla-get-arclength obj_vla)) (setq suffix "m"))
      ((= ent_type "AcDbCircle")
       (if (= measurement_mode "LENGTH")
         (progn (setq prefix "L=") (setq value (* 2 pi (vla-get-radius obj_vla))) (setq suffix "m"))
         (progn (setq prefix "A=") (setq value (vla-get-area obj_vla)) (setq suffix "㎡"))
       ))
      ((= ent_type "AcDbHatch")
       (setq prefix "A=") (setq value (vla-get-area obj_vla)) (setq suffix "㎡"))
      ((= ent_type "AcDbEllipse")
       (if (= measurement_mode "LENGTH")
         (progn (setq prefix "L=") (setq value (vla-get-length obj_vla)) (setq suffix "m"))
         (progn (setq prefix "A=") (setq value (vla-get-area obj_vla)) (setq suffix "㎡"))
       ))
      ((= ent_type "AcDbBlockReference")
       (setq prefix "") (setq value 1) (setq suffix "ea")))
    (if value (list prefix value suffix) nil))

  (defun generate-dynamic-prompt (data_list total_value / prompt_str value_strs all_but_last_strs)
    (if (and data_list (> (length data_list) 0))
      (progn
        (setq value_strs (mapcar '(lambda (d) (rtos (cadr d) 2 2)) data_list))
        (setq all_but_last_strs (if (> (length value_strs) 1) (reverse (cdr (reverse value_strs))) nil))
        (setq prompt_str 
          (strcat 
            (caar data_list)
            (if all_but_last_strs 
              (apply 'strcat (mapcar '(lambda (s) (strcat s (caddar data_list) " + ")) all_but_last_strs))
              ""
            )
            (last value_strs) (caddar data_list)
          )
        )
        (if (> (length data_list) 1)
          (setq prompt_str (strcat prompt_str " = " (rtos total_value 2 2) (caddar data_list)))
        )
        (strcat "\n측정값 '" prompt_str "' 다음객체 선택 (Enter로 완료): ")
      )
      "\n측정할 첫번째 객체를 선택하세요: "
    )
  )

  (defun add-comma-to-number (num_str decimal_places / int_part dec_part result i len dot_pos)
    (setq dot_pos (vl-string-search "." num_str))
    
    (if dot_pos
      (progn
        (setq int_part (substr num_str 1 dot_pos))
        (setq dec_part (substr num_str (+ dot_pos 2)))
      )
      (progn
        (setq int_part num_str)
        (setq dec_part "")
      )
    )
    
    (setq len (strlen int_part))
    (setq result "")
    (setq i 0)
    
    (while (< i len)
      (setq result (strcat result (substr int_part (+ i 1) 1)))
      (setq i (+ i 1))
      (if (and (> (- len i) 0) (= (rem (- len i) 3) 0))
        (setq result (strcat result ","))
      )
    )
    
    (if (> decimal_places 0)
      (setq result (strcat result "." dec_part))
      result
    )
  )

  (defun format-rounded-string (data_list / num rounded_num_as_real old_dimzin final_string num_str)
  (if data_list
    (progn
      (setq old_dimzin (getvar "DIMZIN"))
      (setvar "DIMZIN" 2)
      (setq num (cadr data_list))
      (if (= (caddr data_list) "ea")
        (progn
          (setq num_str (rtos (fix num) 2 0))
          (setq num_str (add-comma-to-number num_str 0))
          (setq final_string (strcat (car data_list) num_str (caddr data_list)))
        )
        (progn
          (setq rounded_num_as_real (atof (rtos num 2 0)))
          (setq num_str (rtos rounded_num_as_real 2 1))
          (setq num_str (add-comma-to-number num_str 1))
          (setq final_string (strcat (car data_list) num_str (caddr data_list)))
        )
      )
      (setvar "DIMZIN" old_dimzin)
      final_string
    ) "" ) )

  (defun add-to-selection-set (obj_vla / obj_ename)
    (setq obj_ename (vlax-vla-object->ename obj_vla))
    (if (not g_sr_selection_set)
      (setq g_sr_selection_set (ssadd))
    )
    (ssadd obj_ename g_sr_selection_set)
    (sssetfirst nil g_sr_selection_set)
  )

  (defun save-current-view (/ current_center current_size)
    (setq current_center (getvar "VIEWCTR"))
    (setq current_size (getvar "VIEWSIZE"))
    (if (listp current_center)
      (setq g_saved_view_center (list (car current_center) (cadr current_center)))
      (setq g_saved_view_center current_center)
    )
    (setq g_viewport_height current_size)
    (prompt (strcat "\n>> 모델 공간 화면 상태 저장 완료 (중심: " 
                   (rtos (car g_saved_view_center) 2 2) "," 
                   (rtos (cadr g_saved_view_center) 2 2)
                   ", 높이: " (rtos g_viewport_height 2 2) ")"))
  )

  (defun restore-saved-view ()
    (if (and g_saved_view_center g_viewport_height)
      (vl-catch-all-apply
        '(lambda ()
           (setq view_center_3d (list (car g_saved_view_center) (cadr g_saved_view_center) 0.0))
           (command "_.zoom" "_c" view_center_3d g_viewport_height)
         )
      )
    )
  )

  (defun save-current-paper-view (/ current_center current_size)
    (setq current_center (getvar "VIEWCTR"))
    (setq current_size (getvar "VIEWSIZE"))
    (if (listp current_center)
      (setq g_saved_paper_view_center (list (car current_center) (cadr current_center)))
      (setq g_saved_paper_view_center current_center)
    )
    (setq g_paper_viewport_height current_size)
    (prompt (strcat "\n>> 배치 공간 화면 상태 저장 완료 (중심: " 
                   (rtos (car g_saved_paper_view_center) 2 2) "," 
                   (rtos (cadr g_saved_paper_view_center) 2 2)
                   ", 높이: " (rtos g_paper_viewport_height 2 2) ")"))
  )

  (defun restore-saved-paper-view ()
    (if (and g_saved_paper_view_center g_paper_viewport_height)
      (vl-catch-all-apply
        '(lambda ()
           (setq view_center_3d (list (car g_saved_paper_view_center) (cadr g_saved_paper_view_center) 0.0))
           (command "_.zoom" "_c" view_center_3d g_paper_viewport_height)
         )
      )
    )
  )

  (defun save-viewport-paper-view (/ current_center current_size)
    (setq current_center (getvar "VIEWCTR")
          current_size (getvar "VIEWSIZE"))
    (if (listp current_center)
      (setq g_saved_paper_view_center (list (car current_center) (cadr current_center)))
      (setq g_saved_paper_view_center current_center)
    )
    (setq g_paper_viewport_height current_size)
    (prompt (strcat "\n>> 뷰포트용 배치 공간 화면 상태 저장 완료 (중심: " 
                   (rtos (car g_saved_paper_view_center) 2 2) "," 
                   (rtos (cadr g_saved_paper_view_center) 2 2)
                   ", 높이: " (rtos g_paper_viewport_height 2 2) ")"))
  )

  (defun restore-viewport-paper-view ()
    (if (and g_saved_paper_view_center g_paper_viewport_height)
      (vl-catch-all-apply
        '(lambda ()
           (setq view_center_3d (list (car g_saved_paper_view_center) (cadr g_saved_paper_view_center) 0.0))
           (command "_.zoom" "_c" view_center_3d g_paper_viewport_height)
         )
      )
    )
  )

  (defun zoom-to-viewport (vp_obj)
    (vl-catch-all-apply
      '(lambda ()
         (setq vp_center_ps (vlax-safearray->list (vlax-variant-value (vla-get-center vp_obj)))
               vp_width_ps (vla-get-width vp_obj)
               vp_height_ps (vla-get-height vp_obj))
         (setq vp_left (- (car vp_center_ps) (/ vp_width_ps 2.0))
               vp_right (+ (car vp_center_ps) (/ vp_width_ps 2.0))
               vp_bottom (- (cadr vp_center_ps) (/ vp_height_ps 2.0))
               vp_top (+ (cadr vp_center_ps) (/ vp_height_ps 2.0)))
         (command "_.zoom" "_window" (list vp_left vp_bottom) (list vp_right vp_top))
         (setq g_paper_viewport_width vp_width_ps)
         (prompt (strcat "\n>> 뷰포트 영역에 맞춰 화면을 조정했습니다. (크기: " 
                        (rtos vp_width_ps 2 2) " x " (rtos vp_height_ps 2 2) ")"))
       )
    )
  )

  ;; ★★★ 수정된 임시 폴리라인 생성 함수 - 임시 측정 전용 레이어 사용 ★★★
  (defun create-temp-polyline (points is_closed / temp_pline coord_list point_array i acad_doc target_space temp_layer_name)
    (setq acad_doc (vla-get-activedocument (vlax-get-acad-object)))
    
    ;; 임시 측정 객체용 레이어 준비
    (setq temp_layer_name (ensure-temp-measurement-layer))
    
    (setq coord_list '())
    (foreach pt points
      (setq coord_list (append coord_list (list (car pt) (cadr pt))))
    )
    (setq point_array (vlax-make-safearray vlax-vbdouble (cons 0 (1- (length coord_list)))))
    (setq i 0)
    (foreach coord coord_list
      (vlax-safearray-put-element point_array i coord)
      (setq i (1+ i))
    )
    (if (or (= (getvar "TILEMODE") 1) (> (getvar "CVPORT") 1))
      (setq target_space (vla-get-modelspace acad_doc))
      (setq target_space (vla-get-paperspace acad_doc))
    )
    (setq temp_pline (vla-addlightweightpolyline 
                       target_space
                       point_array))
    (if is_closed (vla-put-closed temp_pline :vlax-true))
    
    ;; ★★★ 임시 측정 전용 레이어로 설정 ★★★
    (vla-put-layer temp_pline temp_layer_name)
    (vla-put-color temp_pline 2)
    
    temp_pline
  )

  (defun delete-temp-object (temp_ename)
    (if temp_ename
      (vl-catch-all-apply
        '(lambda ()
           (if (and (not (vlax-erased-p (vlax-ename->vla-object temp_ename)))
                    (entget temp_ename))
             (entdel temp_ename)
           )
         )
      )
    )
  )

  (defun delete-all-temp-objects ()
    (if g_temp_measurement_object_list
      (progn
        (foreach temp_obj g_temp_measurement_object_list
          (delete-temp-object temp_obj)
        )
        (setq g_temp_measurement_object_list nil)
        (prompt "\n>> 모든 임시 측정 객체가 삭제되었습니다.")
      )
    )
  )

  (defun perform-manual-measurement (/ points pt continue_input total_length area is_closed
                                      temp_pline temp_ename measurement_data_list total_value
                                      current_temp_pline)
    (setq points '() continue_input t total_length 0.0 area 0.0 is_closed nil current_temp_pline nil)
    (prompt "\n=== 수동 측정 모드 ===")
    (prompt "\n연속으로 점을 클릭하세요. Enter로 완료, C로 폐합:")
    (setq pt (getpoint "\n첫 번째 점을 클릭하세요: "))
    (if pt (setq points (list pt)))
    (while (and continue_input pt)
      (initget "Close")
      (setq pt (getpoint (last points) "\n다음 점을 클릭하세요 [Close(C): 폐합/Enter: 완료]: "))
      (cond
        ((= pt "Close")
         (if (>= (length points) 3)
           (progn
             (setq is_closed t)
             (setq continue_input nil)
             (prompt "\n>> 폐합된 도형으로 완료되었습니다.")
           )
           (prompt "\n>> 폐합하려면 최소 3개의 점이 필요합니다.")
         )
        )
        ((and pt (listp pt))
         (setq points (append points (list pt)))
         (prompt (strcat "\n점 " (itoa (length points)) "개 입력됨"))
         (if current_temp_pline
           (if (not (vlax-erased-p current_temp_pline))
             (vla-delete current_temp_pline)
           )
         )
         (if (>= (length points) 2)
           (setq current_temp_pline (create-temp-polyline points nil))
         )
         (command "_.REDRAW")
        )
        (t
         (setq continue_input nil)
         (if (< (length points) 2)
           (progn
             (prompt "\n>> 최소 2개의 점이 필요합니다.")
             (setq points nil)
           )
         )
        )
      )
    )
    (if (and points (>= (length points) 2))
      (vl-catch-all-apply
        '(lambda ()
           (if (and current_temp_pline (not (vlax-erased-p current_temp_pline)))
             (vla-delete current_temp_pline)
           )
           (setq temp_pline (create-temp-polyline points is_closed))
           (setq temp_ename (vlax-vla-object->ename temp_pline))
           (command "_.REDRAW")
           (if is_closed
             (progn
               (setq area (vla-get-area temp_pline))
               (setq measurement_data_list (list (list "A=" area "㎡")))
               (setq total_value area)
               (prompt (strcat "\n>> 폐합 도형 넓이: " (rtos area 2 2) "㎡"))
             )
             (progn
               (setq total_length (vla-get-length temp_pline))
               (setq measurement_data_list (list (list "L=" total_length "m")))
               (setq total_value total_length)
               (prompt (strcat "\n>> 총 길이: " (rtos total_length 2 2) "m"))
             )
           )
           (list measurement_data_list total_value temp_ename)
         )
      )
      (progn
        (if (and current_temp_pline (not (vlax-erased-p current_temp_pline)))
          (vla-delete current_temp_pline)
        )
        nil
      )
    )
  )

  (defun get-valid-text-object-with-back (prompt_msg space_type / user_input obj_ename obj_vla min_pt max_pt text_center)
    (setq obj_vla nil)
    (while (not obj_vla)
      (initget "Back")
      (setq user_input (entsel (strcat prompt_msg " [Back: 되돌리기]: ")))
      (cond
        ((= user_input "Back")
         (setq obj_vla "BACK"))
        ((and user_input (listp user_input))
         (setq obj_ename (car user_input))
         (if (and (member (cdr (assoc 0 (entget obj_ename))) '("TEXT" "MTEXT"))
                  (if (= space_type "model")
                    (/= (cdr (assoc 67 (entget obj_ename))) 1)
                    (= (cdr (assoc 67 (entget obj_ename))) 1)))
           (progn
             (setq obj_vla (vlax-ename->vla-object obj_ename))
             (vl-catch-all-apply
               '(lambda ()
                  (vla-getboundingbox obj_vla 'min_pt 'max_pt)
                  (setq min_pt (vlax-safearray->list min_pt)
                        max_pt (vlax-safearray->list max_pt))
                  (setq text_center 
                        (list (/ (+ (car min_pt) (car max_pt)) 2.0)
                              (/ (+ (cadr min_pt) (cadr max_pt)) 2.0)))
                  (if (= space_type "paper")
                    (setq g_saved_paper_text_center text_center)
                    (setq g_saved_text_center text_center)
                  )
                  (prompt (strcat "\n>> " 
                                 (if (= space_type "paper") "배치" "모델")
                                 " 공간 화면 중심 저장 (텍스트 중심): " 
                                 (rtos (car text_center) 2 2) "," 
                                 (rtos (cadr text_center) 2 2)))
                )
             )
           )
           (prompt (strcat "\n>> 오류: " 
                          (if (= space_type "model") "모형" "배치")
                          " 공간의 TEXT 또는 MTEXT를 선택하세요."))))
        (t 
         (prompt "\n>> TEXT 또는 MTEXT를 선택하거나 'Back'을 입력하세요."))
      )
    )
    obj_vla
  )

  (defun display-and-input-measurement (prompt_val text_val sel_obj_txt)
    (prompt "\n========================================")
    (prompt (strcat "\n  측정 결과: " prompt_val))
    (prompt "\n========================================")
    (vla-put-textstring sel_obj_txt text_val)
    (prompt (strcat "\n완료: '" text_val "' 값이 입력되었습니다.")))

  (defun clear-selection-set ()
    (if g_sr_selection_set
      (progn
        (setq g_sr_selection_set nil)
        (sssetfirst nil nil)
        (command "_.REDRAW")
      )
    )
  )

  (defun is-compatible-measurement (existing_type new_type current_mode / result)
    (setq result nil)
    (cond
      ((= current_mode "LENGTH")
       (cond
         ((= existing_type "L=")
          (if (= new_type "L=") (setq result t)))
         ((= existing_type "A=")
          (if (= new_type "L=") (setq result t)))
         ((= existing_type "")
          (if (= new_type "") (setq result t)))
         (t (setq result nil))
       )
      )
      (t
       (if (= existing_type new_type) (setq result t))
      )
    )
    result
  )

  (defun perform-object-selection (/ first_sel first_obj_vla first_obj_type manual_result temp_ename
                                     measurement_data_list total_value measurement_type
                                     continue_selection user_sel sel_obj_vla sel_obj_ename new_data prompt_string
                                     current_measurement_mode)
    (initget "D")
    (setq first_sel (entsel "\n측정할 첫번째 객체를 선택하세요 : / D : 수동측정"))
    (cond
      ((= first_sel "D")
       (setq manual_result (perform-manual-measurement))
       (if manual_result
         (progn
           (setq measurement_data_list (car manual_result)
                 total_value (cadr manual_result)
                 measurement_type (caar (car manual_result)))
           (if (not g_temp_measurement_object_list)
             (setq g_temp_measurement_object_list '())
           )
           (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
           (setq current_measurement_mode 
             (cond
               ((= measurement_type "L=") "LENGTH")
               ((= measurement_type "A=") "AREA")
               (t nil)
             )
           )
           (setq continue_selection t)
           (while continue_selection
             (setq prompt_string (generate-dynamic-prompt measurement_data_list total_value))
             (initget "D")
             (setq user_sel (entsel (strcat prompt_string " / D : 수동측정")))
             (cond
               ((not user_sel)
                (setq continue_selection nil))
               ((= user_sel "D")
                (setq manual_result (perform-manual-measurement))
                (if manual_result
                  (progn
                    (setq new_data (car manual_result))
                    (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
                      (progn
                        (setq measurement_data_list (append measurement_data_list new_data)
                              total_value (+ total_value (cadr manual_result)))
                        (if (not g_temp_measurement_object_list)
                          (setq g_temp_measurement_object_list '())
                        )
                        (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
                      )
                      (progn
                        (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                        (delete-temp-object (caddr manual_result))
                      )
                    )
                  )
                )
               )
               ((listp user_sel)
                (setq sel_obj_ename (car user_sel))
                (cond
                  ((member sel_obj_ename g_temp_measurement_object_list)
                   (prompt "\n>> 오류: 수동 측정으로 생성된 임시 객체는 선택할 수 없습니다."))
                  ((and g_sr_selection_set (ssmemb sel_obj_ename g_sr_selection_set))
                   (prompt "\n>> 이미 선택된 객체입니다."))
                  (t
                   (setq sel_obj_vla (vlax-ename->vla-object sel_obj_ename))
                   (setq new_data (get-measurement-data sel_obj_vla current_measurement_mode))
                   (cond
                     ((not new_data)
                      (prompt (strcat "\n>> 오류: 지원되지 않는 객체(" (vla-get-objectname sel_obj_vla) ")입니다.")))
                     ((not (is-compatible-measurement measurement_type (car new_data) current_measurement_mode))
                      (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다."))
                     (t
                      (setq measurement_data_list (append measurement_data_list (list new_data))
                            total_value (+ total_value (cadr new_data)))
                      (add-to-selection-set sel_obj_vla))
                   ))
                ))
             )
           )
           (list measurement_data_list total_value)
         )
         nil
       )
      )
      ((listp first_sel)
        (setq first_obj_vla (vlax-ename->vla-object (car first_sel))
              first_obj_type (vla-get-objectname first_obj_vla))
        (if (= first_obj_type "AcDbBlockReference")
          (process-block-selection first_obj_vla)
          (process-object-selection-only first_obj_vla)
        )
      )
      (t nil)
    )
  )

  (defun perform-object-selection-with-viewport-exit (acad_doc original_mspace_state / 
                                                      first_sel first_obj_vla first_obj_type manual_result
                                                      measurement_data_list total_value measurement_type
                                                      continue_selection user_sel sel_obj_vla sel_obj_ename new_data prompt_string
                                                      current_measurement_mode)
    (initget "D")
    (setq first_sel (entsel "\n측정할 첫번째 객체를 선택하세요 : / D : 수동측정"))
    (cond
      ((= first_sel "D")
       (setq manual_result (perform-manual-measurement))
       (if manual_result
         (progn
           (setq measurement_data_list (car manual_result)
                 total_value (cadr manual_result)
                 measurement_type (caar (car manual_result)))
           (if (not g_temp_measurement_object_list)
             (setq g_temp_measurement_object_list '())
           )
           (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
           (setq current_measurement_mode 
             (cond
               ((= measurement_type "L=") "LENGTH")
               ((= measurement_type "A=") "AREA")
               (t nil)
             )
           )
           (setq continue_selection t)
           (while continue_selection
             (setq prompt_string (generate-dynamic-prompt measurement_data_list total_value))
             (initget "D")
             (setq user_sel (entsel (strcat prompt_string " / D : 수동측정 (ESC: 취소)")))
             (cond
               ((not user_sel)
                (setq continue_selection nil))
               ((= user_sel "D")
                (setq manual_result (perform-manual-measurement))
                (if manual_result
                  (progn
                    (setq new_data (car manual_result))
                    (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
                      (progn
                        (setq measurement_data_list (append measurement_data_list new_data)
                              total_value (+ total_value (cadr manual_result)))
                        (if (not g_temp_measurement_object_list)
                          (setq g_temp_measurement_object_list '())
                        )
                        (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
                      )
                      (progn
                        (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                        (delete-temp-object (caddr manual_result))
                      )
                    )
                  )
                )
               )
               ((listp user_sel)
                (setq sel_obj_ename (car user_sel))
                (cond
                  ((member sel_obj_ename g_temp_measurement_object_list)
                   (prompt "\n>> 오류: 수동 측정으로 생성된 임시 객체는 선택할 수 없습니다."))
                  ((and g_sr_selection_set (ssmemb sel_obj_ename g_sr_selection_set))
                   (prompt "\n>> 이미 선택된 객체입니다."))
                  (t
                   (setq sel_obj_vla (vlax-ename->vla-object sel_obj_ename))
                   (setq new_data (get-measurement-data sel_obj_vla current_measurement_mode))
                   (cond
                     ((not new_data)
                      (prompt (strcat "\n>> 오류: 지원되지 않는 객체(" (vla-get-objectname sel_obj_vla) ")입니다.")))
                     ((not (is-compatible-measurement measurement_type (car new_data) current_measurement_mode))
                      (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다."))
                     (t
                      (setq measurement_data_list (append measurement_data_list (list new_data))
                            total_value (+ total_value (cadr new_data)))
                      (add-to-selection-set sel_obj_vla))
                   ))
                ))
             )
           )
           (list measurement_data_list total_value)
         )
         (progn
           (prompt "\n>> 취소")
           (vla-put-mspace acad_doc original_mspace_state)
           nil
         )
       )
      )
      ((listp first_sel)
        (setq first_obj_vla (vlax-ename->vla-object (car first_sel))
              first_obj_type (vla-get-objectname first_obj_vla))
        (if (= first_obj_type "AcDbBlockReference")
          (process-block-selection-with-viewport-exit first_obj_vla acad_doc original_mspace_state)
          (process-object-selection-only-with-viewport-exit first_obj_vla acad_doc original_mspace_state)
        )
      )
      (t
       (prompt "\n>> 취소")
       (vla-put-mspace acad_doc original_mspace_state)
       nil)
    )
  )

;; =================================Half line for upload================================

  (defun process-object-selection-only (first_obj_vla / continue_selection user_sel sel_obj_vla sel_obj_ename new_data
                                        measurement_data_list total_value measurement_type
                                        prompt_string current_measurement_mode)
    (setq continue_selection t measurement_data_list '() total_value 0.0 measurement_type nil)
    (setq new_data (get-measurement-data first_obj_vla nil))
    (setq measurement_type (car new_data)
          measurement_data_list (list new_data)
          total_value (cadr new_data))
    (setq current_measurement_mode 
      (cond
        ((= (car new_data) "L=") "LENGTH")
        ((= (car new_data) "A=") "AREA")
        ((= (car new_data) "") "COUNT")
        (t nil)
      )
    )
    (add-to-selection-set first_obj_vla)
    (while continue_selection
      (setq prompt_string (generate-dynamic-prompt measurement_data_list total_value))
      (initget "D")
      (setq user_sel (entsel (strcat prompt_string " / D : 수동측정")))
      (cond
        ((not user_sel)
         (setq continue_selection nil))
        ((= user_sel "D")
         (setq manual_result (perform-manual-measurement))
         (if manual_result
           (progn
             (setq new_data (car manual_result))
             (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
               (progn
                 (setq measurement_data_list (append measurement_data_list new_data)
                       total_value (+ total_value (cadr manual_result)))
                 (if (not g_temp_measurement_object_list)
                   (setq g_temp_measurement_object_list '())
                 )
                 (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
               )
               (progn
                 (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                 (delete-temp-object (caddr manual_result))
               )
             )
           )
         )
        )
        ((listp user_sel)
         (setq sel_obj_ename (car user_sel))
         (cond
           ((member sel_obj_ename g_temp_measurement_object_list)
            (prompt "\n>> 오류: 수동 측정으로 생성된 임시 객체는 선택할 수 없습니다."))
           ((and g_sr_selection_set (ssmemb sel_obj_ename g_sr_selection_set))
            (prompt "\n>> 이미 선택된 객체입니다."))
           (t
            (setq sel_obj_vla (vlax-ename->vla-object sel_obj_ename))
            (setq new_data (get-measurement-data sel_obj_vla current_measurement_mode))
            (cond
              ((not new_data)
               (prompt (strcat "\n>> 오류: 지원되지 않는 객체(" (vla-get-objectname sel_obj_vla) ")입니다.")))
              ((not (is-compatible-measurement measurement_type (car new_data) current_measurement_mode))
               (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다."))
              (t
               (if (not measurement_type) (setq measurement_type (car new_data)))
               (setq measurement_data_list (append measurement_data_list (list new_data))
                     total_value (+ total_value (cadr new_data)))
               (add-to-selection-set sel_obj_vla))
            ))
         ))
      )
    )
    (list measurement_data_list total_value)
  )

  (defun process-object-selection-only-with-viewport-exit (first_obj_vla acad_doc original_mspace_state / 
                                                           continue_selection user_sel sel_obj_vla sel_obj_ename new_data
                                                           measurement_data_list total_value measurement_type
                                                           prompt_string current_measurement_mode manual_result)
    (setq continue_selection t measurement_data_list '() total_value 0.0 measurement_type nil)
    (setq new_data (get-measurement-data first_obj_vla nil))
    (setq measurement_type (car new_data)
          measurement_data_list (list new_data)
          total_value (cadr new_data))
    (setq current_measurement_mode 
      (cond
        ((= (car new_data) "L=") "LENGTH")
        ((= (car new_data) "A=") "AREA")
        ((= (car new_data) "") "COUNT")
        (t nil)
      )
    )
    (add-to-selection-set first_obj_vla)
    (while continue_selection
      (setq prompt_string (generate-dynamic-prompt measurement_data_list total_value))
      (initget "D")
      (setq user_sel (entsel (strcat prompt_string " / D : 수동측정 (ESC: 취소)")))
      (cond
        ((not user_sel)
         (setq continue_selection nil))
        ((= user_sel "D")
         (setq manual_result (perform-manual-measurement))
         (if manual_result
           (progn
             (setq new_data (car manual_result))
             (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
               (progn
                 (setq measurement_data_list (append measurement_data_list new_data)
                       total_value (+ total_value (cadr manual_result)))
                 (if (not g_temp_measurement_object_list)
                   (setq g_temp_measurement_object_list '())
                 )
                 (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
               )
               (progn
                 (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                 (delete-temp-object (caddr manual_result))
               )
             )
           )
         )
        )
        ((listp user_sel)
         (setq sel_obj_ename (car user_sel))
         (cond
           ((member sel_obj_ename g_temp_measurement_object_list)
            (prompt "\n>> 오류: 수동 측정으로 생성된 임시 객체는 선택할 수 없습니다."))
           ((and g_sr_selection_set (ssmemb sel_obj_ename g_sr_selection_set))
            (prompt "\n>> 이미 선택된 객체입니다."))
           (t
            (setq sel_obj_vla (vlax-ename->vla-object sel_obj_ename))
            (setq new_data (get-measurement-data sel_obj_vla current_measurement_mode))
            (cond
              ((not new_data)
               (prompt (strcat "\n>> 오류: 지원되지 않는 객체(" (vla-get-objectname sel_obj_vla) ")입니다.")))
              ((not (is-compatible-measurement measurement_type (car new_data) current_measurement_mode))
               (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다."))
              (t
               (if (not measurement_type) (setq measurement_type (car new_data)))
               (setq measurement_data_list (append measurement_data_list (list new_data))
                     total_value (+ total_value (cadr new_data)))
               (add-to-selection-set sel_obj_vla))
            ))
         ))
      )
    )
    (if (and measurement_data_list (> total_value 0))
      (list measurement_data_list total_value)
      nil
    )
  )

  (defun process-block-selection (first_block_vla / blk_name prompt_msg ss i ename obj_vla
                                                   measurement_data_list total_value)
    (setq blk_name (vla-get-effectivename first_block_vla))
    (setq prompt_msg (strcat "\n'" blk_name "' 블록을 모두 선택하세요. 첫번째 블록은 이미 포함되었습니다: "))
    (prompt prompt_msg)
    (setq ss (ssget (list (cons 0 "INSERT") (cons 2 blk_name))))
    (setq measurement_data_list '() total_value 0.0)
    (setq measurement_data_list (list (list "" 1 "ea"))
          total_value 1.0)
    (add-to-selection-set first_block_vla)
    (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq ename (ssname ss i)
                obj_vla (vlax-ename->vla-object ename))
          (if (/= (vla-get-handle obj_vla) (vla-get-handle first_block_vla))
            (progn
               (setq measurement_data_list (append measurement_data_list (list (list "" 1 "ea")))
                    total_value (1+ total_value))
              (add-to-selection-set obj_vla)
            )
          )
          (setq i (1+ i))
        )
      )
    )
    (list measurement_data_list total_value)
  )

  (defun process-block-selection-with-viewport-exit (first_block_vla acad_doc original_mspace_state / 
                                                     blk_name prompt_msg ss i ename obj_vla
                                                     measurement_data_list total_value)
    (setq blk_name (vla-get-effectivename first_block_vla))
    (setq prompt_msg (strcat "\n'" blk_name "' 블록을 모두 선택하세요. 첫번째 블록은 이미 포함되었습니다 (ESC: 취소): "))
    (prompt prompt_msg)
    (setq ss (ssget (list (cons 0 "INSERT") (cons 2 blk_name))))
    (if (not ss)
      (progn
        (prompt "\n>> 취소")
        (clear-selection-set)
        (vla-put-mspace acad_doc original_mspace_state)
        nil)
      (progn
        (setq measurement_data_list '() total_value 0.0)
        (setq measurement_data_list (list (list "" 1 "ea"))
              total_value 1.0)
        (add-to-selection-set first_block_vla)
        (setq i 0)
        (while (< i (sslength ss))
          (setq ename (ssname ss i)
                obj_vla (vlax-ename->vla-object ename))
          (if (/= (vla-get-handle obj_vla) (vla-get-handle first_block_vla))
            (progn
              (setq measurement_data_list (append measurement_data_list (list (list "" 1 "ea")))
                    total_value (1+ total_value))
              (add-to-selection-set obj_vla)
            )
          )
          (setq i (1+ i))
        )
        (list measurement_data_list total_value)
      )
    )
  )

  (defun perform-additional-object-selection (existing_data_list existing_total existing_type /
                                             continue_selection user_sel sel_obj_vla sel_obj_ename new_data
                                             measurement_data_list total_value measurement_type
                                             prompt_string current_measurement_mode manual_result)
    (setq continue_selection t 
          measurement_data_list existing_data_list 
          total_value existing_total 
          measurement_type existing_type)
    (setq current_measurement_mode 
      (cond
        ((= existing_type "L=") "LENGTH")
        ((= existing_type "A=") "AREA")
        ((= existing_type "") "COUNT")
        (t nil)
      )
    )
    (while continue_selection
      (setq prompt_string (generate-dynamic-prompt measurement_data_list total_value))
      (initget "D")
      (setq user_sel (entsel (strcat prompt_string " / D : 수동측정")))
      (cond
        ((not user_sel)
         (setq continue_selection nil))
        ((= user_sel "D")
         (setq manual_result (perform-manual-measurement))
         (if manual_result
           (progn
             (setq new_data (car manual_result))
             (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
               (progn
                 (setq measurement_data_list (append measurement_data_list new_data)
                       total_value (+ total_value (cadr manual_result)))
                 (if (not g_temp_measurement_object_list)
                   (setq g_temp_measurement_object_list '())
                 )
                 (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
               )
               (progn
                 (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                 (delete-temp-object (caddr manual_result))
               )
             )
           )
         )
        )
        ((listp user_sel)
         (setq sel_obj_ename (car user_sel))
         (cond
           ((member sel_obj_ename g_temp_measurement_object_list)
            (prompt "\n>> 오류: 수동 측정으로 생성된 임시 객체는 선택할 수 없습니다."))
           ((and g_sr_selection_set (ssmemb sel_obj_ename g_sr_selection_set))
            (prompt "\n>> 이미 선택된 객체입니다."))
           (t
            (setq sel_obj_vla (vlax-ename->vla-object sel_obj_ename))
            (setq new_data (get-measurement-data sel_obj_vla current_measurement_mode))
            (cond
              ((not new_data)
               (prompt (strcat "\n>> 오류: 지원되지 않는 객체(" (vla-get-objectname sel_obj_vla) ")입니다.")))
              ((not (is-compatible-measurement measurement_type (car new_data) current_measurement_mode))
               (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다."))
              (t
               (if (not measurement_type) (setq measurement_type (car new_data)))
               (setq measurement_data_list (append measurement_data_list (list new_data))
                     total_value (+ total_value (cadr new_data)))
               (add-to-selection-set sel_obj_vla))
            ))
         ))
      )
    )
    (list measurement_data_list total_value)
  )

  (defun perform-additional-object-selection-with-viewport-exit (existing_data_list existing_total existing_type 
                                                                acad_doc original_mspace_state /
                                                                continue_selection user_sel sel_obj_vla sel_obj_ename new_data
                                                                measurement_data_list total_value measurement_type
                                                                prompt_string current_measurement_mode manual_result)
    (setq continue_selection t 
          measurement_data_list existing_data_list 
          total_value existing_total 
          measurement_type existing_type)
    (setq current_measurement_mode 
      (cond
        ((= existing_type "L=") "LENGTH")
        ((= existing_type "A=") "AREA")
        ((= existing_type "") "COUNT")
        (t nil)
      )
    )
    (while continue_selection
      (setq prompt_string (generate-dynamic-prompt measurement_data_list total_value))
      (initget "D")
      (setq user_sel (entsel (strcat prompt_string " / D : 수동측정 (ESC: 취소)")))
      (cond
        ((not user_sel)
         (setq continue_selection nil))
        ((= user_sel "D")
         (setq manual_result (perform-manual-measurement))
         (if manual_result
           (progn
             (setq new_data (car manual_result))
             (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
               (progn
                 (setq measurement_data_list (append measurement_data_list new_data)
                       total_value (+ total_value (cadr manual_result)))
                 (if (not g_temp_measurement_object_list)
                   (setq g_temp_measurement_object_list '())
                 )
                 (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
               )
               (progn
                 (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                 (delete-temp-object (caddr manual_result))
               )
             )
           )
         )
        )
        ((listp user_sel)
         (setq sel_obj_ename (car user_sel))
         (cond
           ((member sel_obj_ename g_temp_measurement_object_list)
            (prompt "\n>> 오류: 수동 측정으로 생성된 임시 객체는 선택할 수 없습니다."))
           ((and g_sr_selection_set (ssmemb sel_obj_ename g_sr_selection_set))
            (prompt "\n>> 이미 선택된 객체입니다."))
           (t
            (setq sel_obj_vla (vlax-ename->vla-object sel_obj_ename))
            (setq new_data (get-measurement-data sel_obj_vla current_measurement_mode))
            (cond
              ((not new_data)
               (prompt (strcat "\n>> 오류: 지원되지 않는 객체(" (vla-get-objectname sel_obj_vla) ")입니다.")))
              ((not (is-compatible-measurement measurement_type (car new_data) current_measurement_mode))
               (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다."))
              (t
               (if (not measurement_type) (setq measurement_type (car new_data)))
               (setq measurement_data_list (append measurement_data_list (list new_data))
                     total_value (+ total_value (cadr new_data)))
               (add-to-selection-set sel_obj_vla))
            ))
         ))
      )
    )
    (if (and measurement_data_list (> total_value 0))
      (list measurement_data_list total_value)
      nil
    )
  )

  ;; --- 되돌리기 및 화면 복원 옵션을 지원하는 메인 측정 함수 ---
  (defun process-multi-selection-with-back (space_type first_run / 
                                            selection_result measurement_data_list total_value
                                            measurement_complete final_data_list final_text_string
                                            text_prompt sel_obj_txt additional_result prompt_string
                                            text_center_3d)

    ;; 첫 실행이 아니고 화면 복원 옵션이 켜져 있을 때만 복원
    (if (and g_restore_view_enabled (not first_run))
      (progn
        (prompt "\n>> 이전 측정 위치로 화면을 복원합니다...")
        (if (= space_type "model")
          (if (and g_saved_view_center g_viewport_height)
            (restore-saved-view)
          )
          (if (and g_saved_paper_view_center g_paper_viewport_height)
            (restore-saved-paper-view)
          )
        )
      )
    )
    
    (setq selection_result (perform-object-selection))
    
    (if selection_result
      (progn
        (setq measurement_data_list (car selection_result)
              total_value (cadr selection_result))
        
        (setq measurement_complete nil)
        (while (not measurement_complete)
          
          (setq final_data_list (list (caar measurement_data_list) total_value (caddar measurement_data_list))
                final_text_string (format-rounded-string final_data_list))
          
          (setq text_prompt (strcat "\n측정값 '" 
                                   (car final_data_list) 
                                   (rtos (cadr final_data_list) 2 2) 
                                   (caddr final_data_list) 
                                   "' 을 입력할 텍스트를 선택하세요"))
          
          ;; 화면 복원 기능이 켜져 있을 때만 화면 상태 저장
          (if g_restore_view_enabled
            (if (= space_type "model")
              (save-current-view)
              (save-current-paper-view)
            )
          )

          ;; 첫 실행이 아니고 화면 복원 옵션이 켜져 있을 때만 텍스트 위치로 복원
          (if (and g_restore_view_enabled (not first_run))
            (if (= space_type "model")
              (if g_saved_text_center
                (progn
                  (prompt "\n>> 이전 텍스트 위치로 화면을 복원합니다...")
                  (setq text_center_3d (list (car g_saved_text_center) (cadr g_saved_text_center) 0.0))
                  (command "_.zoom" "_c" text_center_3d g_viewport_height)
                )
              )
              (if g_saved_paper_text_center
                (progn
                  (prompt "\n>> 이전 텍스트 위치로 화면을 복원합니다...")
                  (setq text_center_3d (list (car g_saved_paper_text_center) (cadr g_saved_paper_text_center) 0.0))
                  (command "_.zoom" "_c" text_center_3d g_paper_viewport_height)
                )
              )
            )
          )
          (setq sel_obj_txt (get-valid-text-object-with-back text_prompt space_type))
          
          (cond
            ((= sel_obj_txt "BACK")
             (prompt "\n>> 되돌리기: 추가 객체를 선택할 수 있습니다. (현재 측정값 보존)")
             (setq additional_result (perform-additional-object-selection 
                                      measurement_data_list 
                                      total_value 
                                      (caar measurement_data_list)))
             (if additional_result
               (progn
                 (setq measurement_data_list (car additional_result)
                       total_value (cadr additional_result))
                 (prompt "\n>> 추가 객체 선택이 완료되었습니다.")
               )
             )
            )
            ((and sel_obj_txt (/= sel_obj_txt "BACK"))
             (setq prompt_string (strcat (car final_data_list) (rtos (cadr final_data_list) 2 2) (caddr final_data_list)))
             (display-and-input-measurement prompt_string final_text_string sel_obj_txt)
             (setq measurement_complete t))
            (t 
             (prompt "\n*텍스트 선택이 취소되었습니다.*")
             (setq measurement_complete t))
          )
        )
      )
      (prompt "\n*객체 선택이 취소되었습니다.*")
    )
    (clear-selection-set)
    (delete-all-temp-objects)
  )

  ;; --- 모형공간 반복 측정 함수 (화면 복원 옵션 추가) ---
  (defun perform-modelspace-measurements (/ continue_work user_choice first_run layer_sep_choice restore_choice)
    (setq continue_work t first_run t)
    (while continue_work
      (prompt "\n----------------------------------------")
      (if first_run
        (progn
          (initget "Y N")
          (setq restore_choice (getkword "\n화면 복원을 하시겠습니까? [예(Y)/아니오(N)] <아니오>: "))
          (if (or (not restore_choice) (= (strcase restore_choice) "N"))
            (setq g_restore_view_enabled nil)
            (setq g_restore_view_enabled t)
          )
          (initget "Yes No")
          (setq layer_sep_choice (getkword "\n레이어 분리를 하시겠습니까? [예(Y)/아니오(N)] <아니오>: "))
          (if (not layer_sep_choice) (setq layer_sep_choice "No"))
          (if (= layer_sep_choice "Yes")
            (perform-layer-separation "model")
          )
        )
      )
      (process-multi-selection-with-back "model" first_run)
      (setq first_run nil)
      (initget "Yes No")
      (setq user_choice (getkword "\n다른 객체를 측정하시겠습니까? [예(Y)/아니오(N)] <예>: "))
      (if (not user_choice) (setq user_choice "Yes"))
      (if (= user_choice "No") (setq continue_work nil))
    )
    (if g_layer_separation_active
      (restore-layer-states)
    )
  )

  ;; --- 배치공간 반복 측정 함수 (화면 복원 옵션 추가) ---
  (defun perform-paperspace-measurements (/ continue_work user_choice first_run layer_sep_choice restore_choice)
    (setq continue_work t first_run t)
    (while continue_work
      (prompt "\n----------------------------------------")
      (if first_run
        (progn
          (initget "Y N")
          (setq restore_choice (getkword "\n화면 복원을 하시겠습니까? [예(Y)/아니오(N)] <아니오>: "))
          (if (or (not restore_choice) (= (strcase restore_choice) "N"))
            (setq g_restore_view_enabled nil)
            (setq g_restore_view_enabled t)
          )
          (initget "Yes No")
          (setq layer_sep_choice (getkword "\n레이어 분리를 하시겠습니까? [예(Y)/아니오(N)] <아니오>: "))
          (if (not layer_sep_choice) (setq layer_sep_choice "No"))
          (if (= layer_sep_choice "Yes")
            (perform-layer-separation "paper")
          )
        )
      )
      (process-multi-selection-with-back "paper" first_run)
      (setq first_run nil)
      (initget "Yes No")
      (setq user_choice (getkword "\n다른 객체를 측정하시겠습니까? [예(Y)/아니오(N)] <예>: "))
      (if (not user_choice) (setq user_choice "Yes"))
      (if (= user_choice "No") (setq continue_work nil))
    )
    (if g_layer_separation_active
      (restore-layer-states)
    )
  )

  ;; --- 뷰포트 측정 함수 (화면 복원 옵션 추가) ---
  (defun perform-viewport-measurements (vp_obj / continue_work user_choice original_mspace_state selection_result
                                                measurement_data_list total_value final_data_list
                                                prompt_string final_text_string sel_obj_txt text_selection_prompt
                                                measurement_complete additional_result first_run layer_separation_choice
                                                restore_choice catch_error text_center_3d)
    (setq continue_work t first_run t)
    (prompt "\n>> 뷰포트 화면으로 줌하여 활성화를 준비합니다...")
    (zoom-to-viewport vp_obj)
    (while continue_work
      (prompt "\n----------------------------------------")
      (if first_run
        (progn
          (initget "Y N")
          (setq restore_choice (getkword "\n화면 복원을 하시겠습니까? [예(Y)/아니오(N)] <아니오>: "))
          (if (or (not restore_choice) (= (strcase restore_choice) "N"))
            (setq g_restore_view_enabled nil)
            (setq g_restore_view_enabled t)
          )
        )
      )
      (if (and g_restore_view_enabled (not first_run))
        (progn
          (prompt "\n>> 이전 측정 위치로 화면을 복원합니다...")
          (restore-viewport-paper-view)
        )
      )
      (setq original_mspace_state (vla-get-mspace acad_doc))
      (if (vl-catch-all-error-p (vl-catch-all-apply
            '(lambda () (vla-put-mspace acad_doc :vlax-true) (vla-put-activepviewport acad_doc vp_obj))))
        (progn 
          (prompt "\n>> 오류: 뷰포트 활성화에 실패했습니다.")
          (setq continue_work nil)
        )
        (progn
          (prompt "\n>> 뷰포트가 활성화되었습니다.")
          (if first_run
            (progn
              (initget "Yes No")
              (setq layer_separation_choice (getkword "\n레이어 분리를 하시겠습니까? [예(Y)/아니오(N)] <아니오>: "))
              (if (not layer_separation_choice) (setq layer_separation_choice "No"))
              (if (= layer_separation_choice "Yes")
                (if (perform-layer-separation "viewport")
                  (prompt "\n>> 레이어 분리가 완료되었습니다. 객체를 선택하세요.")
                  (prompt "\n>> 레이어 분리를 건너뜁니다.")
                )
                (prompt "\n>> 레이어 분리를 건너뜁니다.")
              )
            )
          )
          (prompt "\n모델 공간 객체를 선택하세요.")
          (setq selection_result nil) 
          (if (vl-catch-all-error-p
                (setq catch_error
                      (vl-catch-all-apply
                        '(lambda ()
                           (setq selection_result (perform-object-selection-with-viewport-exit acad_doc original_mspace_state))
                         )
                      )
                )
              )
              (progn
                (prompt (strcat "\n>> 작업이 취소되었습니다. (" (vl-catch-all-error-message catch_error) ")"))
                (vla-put-mspace acad_doc original_mspace_state)
                (setq selection_result nil)
              )
          )
          (if selection_result
            (progn
              (setq measurement_data_list (car selection_result)
                    total_value (cadr selection_result))
              (if (vl-catch-all-error-p (vl-catch-all-apply
                    '(lambda () (vla-put-mspace acad_doc original_mspace_state))))
                (prompt "\n>> 경고: MSPACE 상태 복원에 실패했습니다.")
                (prompt "\n>> 배치 공간으로 돌아왔습니다.")
              )
              (setq measurement_complete nil)
              (while (not measurement_complete)
                (setq final_data_list (list (caar measurement_data_list) total_value (caddar measurement_data_list))
                      final_text_string (format-rounded-string final_data_list))
                (setq text_selection_prompt 
                  (strcat "\n측정값 '" 
                          (car final_data_list) 
                          (rtos (cadr final_data_list) 2 2) 
                          (caddr final_data_list) 
                          "' 을 입력할 텍스트를 선택하세요"))
                
                (if g_restore_view_enabled
                  (save-viewport-paper-view)
                )

                (if (and g_restore_view_enabled (not first_run) g_saved_paper_text_center)
                  (progn
                    (prompt "\n>> 이전 텍스트 위치로 화면을 복원합니다...")
                    (setq text_center_3d (list (car g_saved_paper_text_center) (cadr g_saved_paper_text_center) 0.0))
                    (command "_.zoom" "_c" text_center_3d g_paper_viewport_height)
                  )
                )
                (setq sel_obj_txt (get-valid-text-object-with-back text_selection_prompt "paper"))
                (cond
                  ((= sel_obj_txt "BACK")
                   (prompt "\n>> 되돌리기: 추가 객체를 선택할 수 있습니다. (현재 측정값 보존)")
                   (restore-viewport-paper-view)
                   (vla-put-mspace acad_doc :vlax-true) 
                   (vla-put-activepviewport acad_doc vp_obj)
                   (prompt "\n>> 뷰포트가 다시 활성화되었습니다.")
                   (setq additional_result (perform-additional-object-selection-with-viewport-exit 
                                            measurement_data_list 
                                            total_value 
                                            (caar measurement_data_list)
                                            acad_doc 
                                            original_mspace_state))
                   (vla-put-mspace acad_doc original_mspace_state)
                   (prompt "\n>> 배치 공간으로 돌아왔습니다.")
                   (if additional_result
                     (progn
                       (setq measurement_data_list (car additional_result)
                             total_value (cadr additional_result))
                       (prompt "\n>> 추가 객체 선택이 완료되었습니다.")
                     )
                   )
                  )
                  ((and sel_obj_txt (/= sel_obj_txt "BACK"))
                   (setq prompt_string (strcat (car final_data_list) (rtos (cadr final_data_list) 2 2) (caddr final_data_list)))
                   (display-and-input-measurement prompt_string final_text_string sel_obj_txt)
                   (setq measurement_complete t))
                  (t 
                   (prompt "\n*텍스트 선택이 취소되었습니다.*")
                   (setq measurement_complete t))
                )
              )
            )
            (prompt "\n*객체 선택이 취소되었습니다.*")
          )
          (clear-selection-set)
          (delete-all-temp-objects)
          (setq first_run nil)
          (initget "Yes No")
          (setq user_choice (getkword "\n같은 뷰포트에서 다른 객체를 측정하시겠습니까? [예(Y)/아니오(N)] <예>: "))
          (if (not user_choice) (setq user_choice "Yes"))
          (if (= user_choice "No") (setq continue_work nil))
        )
      )
    )
    (if g_layer_separation_active
      (restore-layer-states)
    )
  )

  ;; --- 메인 프로그램 시작 ---
  (setq old_error *error* *error* sr_error_handler)
  (if (= (getvar "TILEMODE") 1)
    (progn
      (prompt "\n[모형 공간 측정 모드]")
      (perform-modelspace-measurements)
      (prompt "\n>> 모형 공간 측정 작업이 완료되었습니다.")
    )
    (progn
      (initget "Viewport Paperspace")
      (setq mode (getkword "\n작업을 선택하세요 [뷰포트 내부(V)/배치 공간(P)] <뷰포트 내부>: "))
      (if (not mode) (setq mode "Viewport"))
      
      (if (= mode "Viewport")
        (progn
          (prompt "\n[뷰포트 내부 측정 모드]")
          (prompt "\n** 안내: 뷰포트나 폴리선 경계만 선택하세요. **")
          (setq target_vp_obj nil)
          (while (not target_vp_obj)
            (setq user_input (entsel "\n활성화할 뷰포트 또는 폴리선 경계를 선택하세요: "))
            (if user_input
              (progn
                (setq sel_obj_ename (car user_input)
                      sel_obj_type (cdr (assoc 0 (entget sel_obj_ename))))
                (cond
                  ((= sel_obj_type "VIEWPORT")
                   (setq target_vp_obj (vlax-ename->vla-object sel_obj_ename))
                   (prompt "\n>> 성공: 뷰포트가 선택되었습니다."))
                  ((= sel_obj_type "LWPOLYLINE")
                   (if (is-mvo-polyline sel_obj_ename)
                     (progn
                       (setq target_vp_obj (find-viewport-from-polyline sel_obj_ename))
                       (if target_vp_obj
                         (prompt "\n>> 성공: MVO 뷰포트가 선택되었습니다.")
                         (prompt "\n>> 오류: 연결된 뷰포트를 찾을 수 없습니다. 다시 선택하세요.")))
                     (prompt "\n>> 오류: 선택한 폴리선이 MVO 뷰포트 경계가 아닙니다. 다시 선택하세요.")))
                  (t 
                   (prompt "\n>> 오류: VIEWPORT 또는 MVO 폴리선을 선택해야 합니다. 다시 선택하세요."))
                )
              )
            )
          )
          (perform-viewport-measurements target_vp_obj)
          (prompt "\n>> 뷰포트 내부 측정 작업이 완료되었습니다.")
        )
      )
      
      (if (= mode "Paperspace")
        (progn
          (prompt "\n[배치 공간 직접 측정 모드]")
          (perform-paperspace-measurements)
          (prompt "\n>> 배치 공간 측정 작업이 완료되었습니다.")
        )
      )
    )
  )

  (if g_layer_separation_active
    (restore-layer-states)
  )
  (setq *error* old_error)
  (prompt "\n========================================")
  (prompt "\n  SR 명령이 완료되었습니다.")
  (prompt "\n========================================")
  (princ)
)

(prompt "\n'SR' 명령이 로드되었습니다.")
(princ)
