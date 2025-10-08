;; SR.lsp - 레이어 분리 기능이 추가된 객체 측정 리습 (외부참조 지원)
;; 수정 사항: 뷰포트 내부 측정 모드에서 레이어 분리 기능 추가 + 외부참조 레이어 지원

(defun C:SR (/ acad_doc old_error g_sr_selection_set 
               g_saved_view_center g_saved_paper_view_center
               g_viewport_width g_viewport_height g_paper_viewport_width g_paper_viewport_height
               g_temp_measurement_object_list
               ;; 레이어 분리 관련 전역 변수 추가
               g_original_vp_layer_states g_layer_separation_active
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
  (setq g_original_xref_layer_states nil) ; 외부참조 레이어 상태 저장
  (setq g_layer_separation_active nil)

  ;; --- ESC 또는 오류 발생 시 실행될 안전 장치(오류 핸들러) ---
  (defun sr_error_handler (msg)
    ;; 선택집합 해제
    (if g_sr_selection_set
      (progn
        (setq g_sr_selection_set nil) ; 선택집합 해제
        (vl-catch-all-apply '(lambda () (command-s "_.REDRAW"))) ; 화면 갱신으로 선택 강조 제거
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

  ;; MVO 뷰포트 처리를 위한 함수 (SRQ에서 가져옴)
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

  ;; MVO 뷰포트인지 확인하는 함수 (SRQ에서 가져옴)
  (defun is-mvo-polyline (pline_ename / result)
    (setq result (find-viewport-from-polyline pline_ename))
    (if result t nil))

  ;; 중복 제거 함수 (SRQ에서 가져옴)
  (defun remove-duplicates (lst / result)
    (setq result '())
    (foreach item lst
      (if (not (member item result))
        (setq result (cons item result))))
    (reverse result))

  ;; 레이어 목록을 문자열로 변환하는 함수 (SRQ에서 가져옴)
  (defun layers-to-string (layer_list / result)
    (setq result "")
    (foreach layer layer_list
      (setq result (strcat result "'" layer "' ")))
    result)

  ;; 외부참조 레이어 이름에서 기본 레이어명 추출 함수
  (defun extract-base-layer-name (full_layer_name / pipe_pos)
    (setq pipe_pos (vl-string-search "|" full_layer_name))
    (if pipe_pos
      (substr full_layer_name (+ pipe_pos 2)) ; 파이프 뒤의 레이어명 반환
      full_layer_name ; 외부참조가 아닌 경우 원래 이름 반환
    )
  )

  ;; 외부참조 레이어 확인 함수
  (defun is-xref-layer (layer_name)
    (if (vl-string-search "|" layer_name) t nil)
  )

  ;; 외부참조 레이어 매칭 함수
  (defun is-layer-match (target_layer current_layer / base_target base_current)
    (setq base_target (extract-base-layer-name target_layer)
          base_current (extract-base-layer-name current_layer))
    (or (= target_layer current_layer)                    ; 완전 일치
        (= base_target base_current)                      ; 기본 레이어명 일치
        (and (vl-string-search "|" current_layer)         ; 현재가 외부참조 레이어이고
             (= base_target base_current))                ; 기본명이 일치
        (and (vl-string-search "|" target_layer)          ; 선택된 것이 외부참조이고
             (= base_target base_current))                ; 기본명이 일치
    )
  )

  ;; 레이어 분리 함수 (외부참조 지원 강화)
  (defun perform-layer-separation (/ sel_set target_layer_names layers layer_obj layer_name
                                      paperspace_layers ss i ent layer_list_str obj_vla
                                      xref_layer_names all_layer_names xrefs_collection xref_obj
                                      xref_blocks xref_block_obj safe_layer_name)
    (prompt "\n=== 레이어 분리 모드 (XREF 완전 지원) ===")
    (prompt "\n표시할 레이어의 객체들을 선택하세요 (Enter로 완료): ")
    
    ;; ★★★ XREF 가시성을 위한 시스템 변수 확인 및 설정 ★★★
    (vl-catch-all-apply
      '(lambda ()
         ;; XREF 표시 관련 시스템 변수들 확인
         (if (< (getvar "VISRETAIN") 1) (setvar "VISRETAIN" 1))
         (if (< (getvar "XREFOVERRIDE") 1) (setvar "XREFOVERRIDE" 1))
         (prompt "\n>> XREF 가시성 설정 최적화 완료")
      )
    )
    
    (setq sel_set (ssget))
    
    (if sel_set
      (progn
        ;; 레이어 컬렉션 초기화 (모든 경우에서 필요)
        (setq layers (vla-get-layers acad_doc))
        
        ;; 선택된 객체들의 레이어 수집
        (setq target_layer_names '() i 0)
        (repeat (sslength sel_set)
          (setq obj_vla (vlax-ename->vla-object (ssname sel_set i))
                layer_name (vla-get-layer obj_vla))
          (if (not (member layer_name target_layer_names))
            (setq target_layer_names (cons layer_name target_layer_names)))
          (setq i (1+ i)))
        
        ;; 중복 제거
        (setq target_layer_names (remove-duplicates target_layer_names))
        
        ;; ★★★ 선택적 외부참조 레이어 확장 처리 (개선) ★★★
        ;; 선택된 객체 중에 외부참조 레이어가 있는지 확인
        (setq has_xref_selection nil)
        (foreach layer_name target_layer_names
          (if (is-xref-layer layer_name)
            (setq has_xref_selection t)
          )
        )
        
        ;; 외부참조 객체를 선택한 경우에만 관련 XREF 레이어 확장
        (if has_xref_selection
          (progn
            (prompt "\n>> 외부참조 객체가 선택되어 관련 XREF 레이어를 확장합니다...")
            ;; 모든 도면층을 검사하여 관련 외부참조 레이어 찾기
            (setq all_layer_names '())
            (vlax-for layer_obj layers
              (setq all_layer_names (cons (vla-get-name layer_obj) all_layer_names)))
            
            ;; 선택된 레이어와 매칭되는 모든 레이어 찾기 (외부참조 포함)
            (setq xref_layer_names '())
            (foreach target_layer target_layer_names
              (foreach all_layer all_layer_names
                (if (is-layer-match target_layer all_layer)
                  (if (not (member all_layer xref_layer_names))
                    (setq xref_layer_names (cons all_layer xref_layer_names))
                  )
                )
              )
            )
            
            ;; 최종 표시할 레이어 목록 (원래 + 외부참조 매칭)
            (setq target_layer_names (remove-duplicates (append target_layer_names xref_layer_names)))
          )
          (progn
            (prompt "\n>> 일반 레이어만 선택되어 XREF 확장을 건너뜁니다...")
            ;; 외부참조 선택이 없으면 원래 레이어만 사용
          )
        )
        
        ;; 레이어 목록을 문자열로 변환
        (setq layer_list_str (layers-to-string target_layer_names))
        (prompt (strcat "\n선택된 객체들의 레이어: " layer_list_str))
        (if has_xref_selection
          (prompt (strcat "\n외부참조 확장 포함 총 " (itoa (length target_layer_names)) "개 레이어가 표시됩니다."))
          (prompt (strcat "\n총 " (itoa (length target_layer_names)) "개 레이어가 표시됩니다."))
        )

        ;; 레이어 분리 실행
        (setq g_original_vp_layer_states nil
              g_original_xref_layer_states nil)

        ;; ★★★ 현재 레이어 상태 저장 (On/Off, Freeze/Thaw, Lock/Unlock) - 오류 방지 ★★★
        (vlax-for layer_obj layers
          (vl-catch-all-apply
            '(lambda ()
               (setq layer_name (vla-get-name layer_obj))
               ;; 외부참조 레이어는 안전하게 처리
               (setq safe_layer_name layer_name)
               (setq g_original_vp_layer_states
                 (cons (list safe_layer_name 
                            (vla-get-layeron layer_obj)
                            (vla-get-freeze layer_obj)
                            (vla-get-lock layer_obj))
                       g_original_vp_layer_states))
            )
          )
        )
        
        ;; ★★★ 외부참조 가시성 처리 (강화된 XREF 지원) ★★★
        (vl-catch-all-apply
          '(lambda ()
             ;; 1. XREF 블록 가시성 제어
             (setq xrefs_collection (vla-get-blocks acad_doc))
             (vlax-for xref_block_obj xrefs_collection
               (if (= (vla-get-isxref xref_block_obj) :vlax-true)
                 (vl-catch-all-apply
                   '(lambda ()
                      (setq g_original_xref_layer_states
                        (cons (list (vla-get-name xref_block_obj) 
                                   (vla-get-visible xref_block_obj))
                              g_original_xref_layer_states))
                      ;; XREF 블록을 보이게 설정
                      (vla-put-visible xref_block_obj :vlax-true)
                      (prompt (strcat "\n>> XREF 블록 활성화: " (vla-get-name xref_block_obj)))
                   )
                 )
               )
             )
             
             ;; 2. XREF 재로드 시도 (중요!)
             (prompt "\n>> XREF 객체 새로고침을 위해 REGEN을 실행합니다...")
             (vl-catch-all-apply '(lambda () (command-s "_.REGEN")))
             
             ;; 3. XCLIP 확인 및 해제 시도
             (prompt "\n>> XREF 클리핑 상태를 확인합니다...")
             (vl-catch-all-apply '(lambda () (command-s "_.XCLIP" "_ALL" "_Delete" "")))
          )
        )

        ;; 페이퍼스페이스 레이어 목록 생성
        (setq paperspace_layers '("0"))
        (if (setq ss (ssget "_X" (list '(0 . "*") (cons 410 (getvar "CTAB")))))
          (progn
            (setq i 0)
            (repeat (sslength ss)
              (setq ent (ssname ss i) layer_name (cdr (assoc 8 (entget ent))))
              (if (not (member layer_name paperspace_layers))
                (setq paperspace_layers (cons layer_name paperspace_layers)))
              (setq i (1+ i)))))
        
        ;; ★★★ 레이어 끄기/켜기 처리 (외부참조 고려 + 동결 해제) - 오류 방지 ★★★
        (prompt (strcat "\n[DEBUG] 레이어 제어 시작 - 선택된 레이어: " (vl-princ-to-string target_layer_names)))
        (prompt (strcat "\n[DEBUG] 페이퍼스페이스 레이어: " (vl-princ-to-string paperspace_layers)))
        
        (vlax-for layer_obj layers
          (vl-catch-all-apply
            '(lambda ()
               (setq layer_name (vla-get-name layer_obj))
               (cond
                 ;; 선택된 객체들의 레이어는 켜기 (외부참조 포함)
                 ((member layer_name target_layer_names)
                  (prompt (strcat "\n[DEBUG] 켜는 레이어: " layer_name))
                  (vla-put-layeron layer_obj :vlax-true)
                  ;; 외부참조가 아닌 일반 레이어만 freeze/lock 제어
                  (if (not (vl-string-search "|" layer_name))
                    (progn
                      (vla-put-freeze layer_obj :vlax-false)
                      (vla-put-lock layer_obj :vlax-false)
                      (prompt (strcat "\n[DEBUG] " layer_name " 동결해제/잠금해제 완료"))
                    )
                  )
                 )
                 ;; 페이퍼스페이스 레이어는 그대로 유지
                 ((member layer_name paperspace_layers)
                  (prompt (strcat "\n[DEBUG] 페이퍼스페이스 레이어 유지: " layer_name))
                  nil) ; 상태 유지
                 ;; 나머지 레이어는 끄기 (외부참조 레이어가 아닌 경우만)
                 ((not (vl-string-search "|" layer_name))
                  (prompt (strcat "\n[DEBUG] 끄는 레이어: " layer_name))
                  (vla-put-layeron layer_obj :vlax-false))
                 ;; 외부참조 레이어는 건너뜀
                 (t
                  (prompt (strcat "\n[DEBUG] XREF 레이어 건너뜀: " layer_name)))
               )
            )
          )
        )
        
        (setq g_layer_separation_active t)
        (prompt (strcat "\n>> 레이어 분리 완료: " layer_list_str "만 표시됩니다."))
        (prompt "\n>> 외부참조 객체 가시성도 함께 제어되었습니다.")
        
        ;; ★★★ 최종 XREF 가시성 강제 새로고침 ★★★
        (vl-catch-all-apply
          '(lambda ()
             (prompt "\n>> 최종 화면 새로고침을 수행합니다...")
             (command-s "_.REGEN")
             ;; XREF 레이어의 객체들을 강제로 다시 표시하기 위한 추가 처리
             (foreach xref_layer target_layer_names
               (if (vl-string-search "|" xref_layer)
                 (prompt (strcat "\n>> XREF 레이어 활성화 완료: " xref_layer))
               )
             )
          )
        )
        
        t ; 성공
      )
      (progn
        (prompt "\n>> 레이어 분리가 취소되었습니다.")
        nil ; 취소
      )
    )
  )

  ;; 레이어 상태 복원 함수 (XREF 지원)
  (defun restore-layer-states (/ layer_obj layers xrefs_collection xref_block_obj)
    (if (and g_original_vp_layer_states g_layer_separation_active)
      (progn
        (prompt "\n>> 레이어 상태를 복원하는 중...")
        (setq layers (vla-get-layers acad_doc))
        
        ;; 기본 레이어 상태 복원 (On/Off, Freeze/Thaw, Lock/Unlock)
        (foreach state g_original_vp_layer_states
          (if (not (vl-catch-all-error-p 
                     (vl-catch-all-apply 'vla-item (list layers (car state)))))
            (vl-catch-all-apply
              '(lambda ()
                (setq layer_obj (vla-item layers (car state)))
                (vla-put-layeron layer_obj (cadr state))     ; On/Off 상태
                (vla-put-freeze layer_obj (caddr state))     ; Freeze/Thaw 상태
                (vla-put-lock layer_obj (cadddr state))      ; Lock/Unlock 상태
              )
            )
          )
        )
        
        ;; XREF 블록 가시성 상태 복원
        (if g_original_xref_layer_states
          (vl-catch-all-apply
            '(lambda ()
               (setq xrefs_collection (vla-get-blocks acad_doc))
               (foreach xref_state g_original_xref_layer_states
                 (if (not (vl-catch-all-error-p 
                           (vl-catch-all-apply 'vla-item (list xrefs_collection (car xref_state)))))
                   (vl-catch-all-apply
                     '(lambda ()
                       (setq xref_block_obj (vla-item xrefs_collection (car xref_state)))
                       (vla-put-visible xref_block_obj (cadr xref_state))
                     )
                   )
                 )
               )
            )
          )
        )
        
        ;; 화면 새로고침
        (vl-catch-all-apply '(lambda () (command-s "_.REGEN")))
        
        (setq g_original_vp_layer_states nil
              g_original_xref_layer_states nil
              g_layer_separation_active nil)
        (prompt "\n>> 레이어 상태가 복원되었습니다.")
      )
    )
  )

  ;; --- 기존 측정 데이터 함수 (길이 합산 모드 지원) ---
  (defun get-measurement-data (obj_vla measurement_mode / ent_data ent_type value prefix suffix)
    (setq ent_type (vla-get-objectname obj_vla))
    (setq value nil prefix nil suffix nil)
    (cond
      ((= ent_type "AcDbPolyline")
       (setq ent_data (entget (vlax-vla-object->ename obj_vla)))
       (if (and ent_data (= 1 (logand 1 (cdr (assoc 70 ent_data)))))
         ;; 폐합된 폴리라인
         (if (= measurement_mode "LENGTH") ; 길이 합산 모드일 때
           (progn (setq prefix "L=") (setq value (vla-get-length obj_vla)) (setq suffix "m"))
           (progn (setq prefix "A=") (setq value (vla-get-area obj_vla)) (setq suffix "㎡"))
         )
         ;; 열린 폴리라인
         (progn (setq prefix "L=") (setq value (vla-get-length obj_vla)) (setq suffix "m"))
       ))
      ((= ent_type "AcDbLine")
       (setq prefix "L=") (setq value (vla-get-length obj_vla)) (setq suffix "m"))
      ((= ent_type "AcDbArc")
       (setq prefix "L=") (setq value (vla-get-arclength obj_vla)) (setq suffix "m"))
      ((= ent_type "AcDbCircle")
       (if (= measurement_mode "LENGTH") ; 길이 합산 모드일 때
         (progn (setq prefix "L=") (setq value (* 2 pi (vla-get-radius obj_vla))) (setq suffix "m"))
         (progn (setq prefix "A=") (setq value (vla-get-area obj_vla)) (setq suffix "㎡"))
       ))
      ((= ent_type "AcDbHatch")
       (setq prefix "A=") (setq value (vla-get-area obj_vla)) (setq suffix "㎡"))
      ((= ent_type "AcDbEllipse")
       (if (= measurement_mode "LENGTH") ; 길이 합산 모드일 때
         (progn (setq prefix "L=") (setq value (vla-get-length obj_vla)) (setq suffix "m"))
         (progn (setq prefix "A=") (setq value (vla-get-area obj_vla)) (setq suffix "㎡"))
       ))
      ((= ent_type "AcDbBlockReference")
       (setq prefix "") (setq value 1) (setq suffix "ea")))
    (if value (list prefix value suffix) nil))

  ;; --- 합산 과정을 포함한 동적 프롬프트 문자열 생성 함수 ---
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

  ;; --- DIMZIN 변수를 제어하여 정수 단위로 반올림하여 최종 출력 문자열 생성 ---
  (defun format-rounded-string (data_list / num rounded_num_as_real old_dimzin final_string)
  (if data_list
    (progn
      (setq old_dimzin (getvar "DIMZIN"))
      (setvar "DIMZIN" 2)
      (setq num (cadr data_list))
      
      ;; suffix가 "ea"인 경우에만 정수로 처리
      (if (= (caddr data_list) "ea")
        (setq final_string (strcat (car data_list) (itoa (fix num)) (caddr data_list)))
        ;; 그 외의 경우 (길이/넓이)는 기존 방식 그대로
        (progn
          (setq rounded_num_as_real (atof (rtos num 2 0)))
          (setq final_string (strcat (car data_list) (rtos rounded_num_as_real 2 1) (caddr data_list)))
        )
      )
      
      (setvar "DIMZIN" old_dimzin)
      final_string
    ) "" ) )

  ;; --- 선택집합에 객체 추가 함수 ---
  (defun add-to-selection-set (obj_vla / obj_ename)
    (setq obj_ename (vlax-vla-object->ename obj_vla))
    (if (not g_sr_selection_set)
      (setq g_sr_selection_set (ssadd))
    )
    (ssadd obj_ename g_sr_selection_set)
    ;; 선택집합을 AutoCAD의 현재 선택집합으로 설정하여 강조 표시
    (sssetfirst nil g_sr_selection_set)
  )

  ;; --- 현재 화면 상태 저장 함수 (모델 공간용) - 중심점과 크기 저장 ---
  (defun save-current-view (/ current_center current_size)
    (setq current_center (getvar "VIEWCTR"))
    (setq current_size (getvar "VIEWSIZE"))
    
    ;; VIEWCTR을 리스트로 변환 (3D 점 -> 2D 리스트)
    (if (listp current_center)
      (setq g_saved_view_center (list (car current_center) (cadr current_center)))
      (setq g_saved_view_center current_center)
    )
    
    ;; VIEWSIZE를 g_viewport_height에 저장
    (setq g_viewport_height current_size)
    
    (prompt (strcat "\n>> 모델 공간 화면 상태 저장 완료 (중심: " 
                   (rtos (car g_saved_view_center) 2 2) "," 
                   (rtos (cadr g_saved_view_center) 2 2)
                   ", 높이: " (rtos g_viewport_height 2 2) ")"))
  )

  ;; --- 저장된 화면으로 복원 함수 (모델 공간용) - ZOOM Center 방식 ---
  (defun restore-saved-view ()
    (prompt "\n[DEBUG] restore-saved-view() 시작")
    (prompt (strcat "\n[DEBUG] 중심점: " (vl-princ-to-string g_saved_view_center)))
    (prompt (strcat "\n[DEBUG] 높이: " (if g_viewport_height (rtos g_viewport_height 2 4) "nil")))
    
    (if (and g_saved_view_center g_viewport_height)
      (vl-catch-all-apply
        '(lambda ()
           (prompt "\n[DEBUG] ZOOM Center 방식으로 화면 복원 시작...")
           
           ;; 2D 좌표를 3D 좌표로 변환
           (setq view_center_3d (list (car g_saved_view_center) (cadr g_saved_view_center) 0.0))
           
           (prompt (strcat "\n[DEBUG] 3D 좌표: " (vl-princ-to-string view_center_3d)))
           (prompt (strcat "\n[DEBUG] 높이 값: " (rtos g_viewport_height 2 4)))
           
           ;; ZOOM Center 명령 실행 (정확한 방식)
           (vl-catch-all-apply '(lambda () (command-s "_.zoom" "_c" view_center_3d g_viewport_height)))
           
           (prompt "\n>> 이전 화면으로 복원 완료 (ZOOM Center 방식)")
         )
      )
      (prompt "\n>> 저장된 화면이 없거나 화면 크기 정보가 부족합니다.")
    )
  )

  ;; --- 배치 공간 화면 상태 저장 함수 (수정) - 중심점과 크기 저장 ---
  (defun save-current-paper-view (/ current_center current_size)
    (setq current_center (getvar "VIEWCTR"))
    (setq current_size (getvar "VIEWSIZE"))
    
    ;; VIEWCTR을 리스트로 변환 (3D 점 -> 2D 리스트)
    (if (listp current_center)
      (setq g_saved_paper_view_center (list (car current_center) (cadr current_center)))
      (setq g_saved_paper_view_center current_center)
    )
    
    ;; VIEWSIZE를 g_paper_viewport_height에 저장
    (setq g_paper_viewport_height current_size)
    
    (prompt (strcat "\n>> 배치 공간 화면 상태 저장 완료 (중심: " 
                   (rtos (car g_saved_paper_view_center) 2 2) "," 
                   (rtos (cadr g_saved_paper_view_center) 2 2)
                   ", 높이: " (rtos g_paper_viewport_height 2 2) ")"))
  )

  ;; --- 저장된 배치 공간 화면으로 복원 함수 (수정) - ZOOM Center 방식 ---
  (defun restore-saved-paper-view ()
    (prompt "\n[DEBUG] restore-saved-paper-view() 시작")
    (prompt (strcat "\n[DEBUG] 중심점: " (vl-princ-to-string g_saved_paper_view_center)))
    (prompt (strcat "\n[DEBUG] 높이: " (if g_paper_viewport_height (rtos g_paper_viewport_height 2 4) "nil")))
    
    (if (and g_saved_paper_view_center g_paper_viewport_height)
      (vl-catch-all-apply
        '(lambda ()
           (prompt "\n[DEBUG] 배치공간 ZOOM Center 방식으로 화면 복원 시작...")
           
           ;; 2D 좌표를 3D 좌표로 변환
           (setq view_center_3d (list (car g_saved_paper_view_center) (cadr g_saved_paper_view_center) 0.0))
           
           (prompt (strcat "\n[DEBUG] 3D 좌표: " (vl-princ-to-string view_center_3d)))
           (prompt (strcat "\n[DEBUG] 높이 값: " (rtos g_paper_viewport_height 2 4)))
           
           ;; ZOOM Center 명령 실행
           (vl-catch-all-apply '(lambda () (command-s "_.zoom" "_c" view_center_3d g_paper_viewport_height)))
           
           (prompt "\n>> 이전 배치 공간 화면으로 복원 완료 (ZOOM Center 방식)")
         )
      )
      (prompt "\n>> 저장된 배치 공간 화면이 없거나 화면 크기 정보가 부족합니다.")
    )
  )

  ;; --- 뷰포트용 화면 상태 저장 함수 (새로 추가) ---
  (defun save-viewport-paper-view (/ current_center current_size)
    (setq current_center (getvar "VIEWCTR")
          current_size (getvar "VIEWSIZE")) ; 현재 화면 높이 가져오기
    
    ;; VIEWCTR을 리스트로 변환 (3D 점 -> 2D 리스트)
    (if (listp current_center)
      (setq g_saved_paper_view_center (list (car current_center) (cadr current_center)))
      (setq g_saved_paper_view_center current_center)
    )
    
    ;; 현재 화면 높이를 전역 변수에 저장
    (setq g_paper_viewport_height current_size)
    
    (prompt (strcat "\n>> 뷰포트용 배치 공간 화면 상태 저장 완료 (중심: " 
                   (rtos (car g_saved_paper_view_center) 2 2) "," 
                   (rtos (cadr g_saved_paper_view_center) 2 2)
                   ", 높이: " (rtos g_paper_viewport_height 2 2) ")"))
  )

  ;; --- 뷰포트용 화면 복원 함수 (새로 추가) ---
  (defun restore-viewport-paper-view ()
    (prompt "\n[DEBUG] restore-viewport-paper-view() 시작")
    (prompt (strcat "\n[DEBUG] 중심점: " (vl-princ-to-string g_saved_paper_view_center)))
    (prompt (strcat "\n[DEBUG] 뷰포트 높이: " (if g_paper_viewport_height (rtos g_paper_viewport_height 2 4) "nil")))
    
    (if (and g_saved_paper_view_center g_paper_viewport_height)
      (vl-catch-all-apply
        '(lambda ()
           (prompt "\n[DEBUG] 뷰포트용 배치공간 ZOOM Center 방식으로 화면 복원 시작...")
           
           ;; 2D 좌표를 3D 좌표로 변환
           (setq view_center_3d (list (car g_saved_paper_view_center) (cadr g_saved_paper_view_center) 0.0))
           
           (prompt (strcat "\n[DEBUG] 3D 좌표: " (vl-princ-to-string view_center_3d)))
           (prompt (strcat "\n[DEBUG] 뷰포트 높이 값: " (rtos g_paper_viewport_height 2 4)))
           
           ;; ZOOM Center 명령 실행 - 뷰포트 높이 사용
           (vl-catch-all-apply '(lambda () (command-s "_.zoom" "_c" view_center_3d g_paper_viewport_height)))
           
           (prompt "\n>> 뷰포트용 배치 공간 화면으로 복원 완료 (ZOOM Center 방식)")
         )
      )
      (prompt "\n>> 저장된 뷰포트용 배치 공간 화면이 없거나 뷰포트 크기 정보가 부족합니다.")
    )
  )

  ;; --- 뷰포트 전체로 줌인하고 기준 크기 저장하는 함수 (수정) ---
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
         
         ;; ZOOM Window 실행
         (vl-catch-all-apply '(lambda () (command-s "_.zoom" "_window" (list vp_left vp_bottom) (list vp_right vp_top))))
         
         ;; 뷰포트 너비 정보만 저장 (높이는 동적으로 저장되므로)
         (setq g_paper_viewport_width vp_width_ps)
         
         (prompt (strcat "\n>> 뷰포트 영역에 맞춰 화면을 조정했습니다. (크기: " 
                        (rtos vp_width_ps 2 2) " x " (rtos vp_height_ps 2 2) ")"))
       )
    )
  )

  ;; --- 수정된 임시 폴리라인 생성 함수 ---
  (defun create-temp-polyline (points is_closed / temp_pline coord_list point_array i acad_doc target_space)
    (setq acad_doc (vla-get-activedocument (vlax-get-acad-object)))
    ;; 2D 좌표 배열 생성
    (setq coord_list '())
    (foreach pt points
      (setq coord_list (append coord_list (list (car pt) (cadr pt))))
    )
    
    ;; SafeArray 생성
    (setq point_array (vlax-make-safearray vlax-vbdouble (cons 0 (1- (length coord_list)))))
    
    ;; 좌표 데이터를 SafeArray에 입력
    (setq i 0)
    (foreach coord coord_list
      (vlax-safearray-put-element point_array i coord)
      (setq i (1+ i))
    )
    
    ;; 현재 활성 공간(모형/배치/뷰포트)을 확인하여 객체를 생성할 공간을 결정
    (if (or (= (getvar "TILEMODE") 1) (> (getvar "CVPORT") 1))
      ;; 모형 공간(TILEMODE=1)이거나 활성 뷰포트 내부(CVPORT > 1)인 경우
      (setq target_space (vla-get-modelspace acad_doc))
      ;; 배치 공간 자체(TILEMODE=0 이고 CVPORT=1)인 경우
      (setq target_space (vla-get-paperspace acad_doc))
    )
    
    ;; 경량 폴리라인 생성
    (setq temp_pline (vla-addlightweightpolyline 
                       target_space
                       point_array))
    
    ;; 폐합 설정
    (if is_closed (vla-put-closed temp_pline :vlax-true))
    
    ;; 노란색으로 설정
    (vla-put-color temp_pline 2) ; 노란색 = 2
    
    temp_pline
  )

  ;; --- 수정된 임시 객체 삭제 함수 ---
  (defun delete-temp-object (temp_ename)
    (if temp_ename
      (vl-catch-all-apply
        '(lambda ()
           (if (and (not (vlax-erased-p (vlax-ename->vla-object temp_ename)))
                    (entget temp_ename))
             (progn
               (entdel temp_ename)
               ;; 개별 삭제 메시지는 제거 (너무 많은 메시지 방지)
             )
           )
         )
      )
    )
  )

  ;; --- 모든 임시 객체 삭제 함수 ---
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

  ;; --- 수정된 수동 다점 측정 함수 (실시간 그리기 지원) ---
  (defun perform-manual-measurement (/ points pt continue_input total_length area is_closed
                                      temp_pline temp_ename measurement_data_list total_value
                                      current_temp_pline)
    (setq points '() continue_input t total_length 0.0 area 0.0 is_closed nil current_temp_pline nil)
    
    (prompt "\n=== 수동 측정 모드 ===")
    (prompt "\n연속으로 점을 클릭하세요. Enter로 완료, C로 폐합:")
    
    ;; 첫 번째 점
    (setq pt (getpoint "\n첫 번째 점을 클릭하세요: "))
    (if pt (setq points (list pt)))
    
    ;; 연속 점 입력 (실시간 그리기)
    (while (and continue_input pt)
      (initget "Close")
      (setq pt (getpoint (last points) "\n다음 점을 클릭하세요 [Close(C): 폐합/Enter: 완료]: "))
      
      (cond
        ;; "Close" 또는 "C" 입력 시 폐합
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
        
        ;; 점을 클릭한 경우
        ((and pt (listp pt))
         (setq points (append points (list pt)))
         (prompt (strcat "\n점 " (itoa (length points)) "개 입력됨"))
         
         ;; 실시간 임시 폴리라인 업데이트
         (if current_temp_pline
           (progn
             (if (not (vlax-erased-p current_temp_pline))
               (vla-delete current_temp_pline)
             )
           )
         )
         (if (>= (length points) 2)
           (setq current_temp_pline (create-temp-polyline points nil))
         )
         (vl-catch-all-apply '(lambda () (command-s "_.REDRAW")))
        )
        
        ;; Enter 또는 취소
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
    
    ;; 점이 2개 이상일 때만 처리
    (if (and points (>= (length points) 2))
      (vl-catch-all-apply
        '(lambda ()
           ;; 기존 임시 폴리라인 삭제
           (if (and current_temp_pline (not (vlax-erased-p current_temp_pline)))
             (vla-delete current_temp_pline)
           )
           
           ;; 최종 임시 폴리라인 생성
           (setq temp_pline (create-temp-polyline points is_closed))
           (setq temp_ename (vlax-vla-object->ename temp_pline))
           
           ;; 화면 갱신
           (vl-catch-all-apply '(lambda () (command-s "_.REDRAW")))
           
           ;; 길이/넓이 계산
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
           
           ;; 결과 반환 (임시 객체는 나중에 삭제)
           (list measurement_data_list total_value temp_ename)
         )
      )
      (progn
        ;; 실패 시 임시 객체 정리
        (if (and current_temp_pline (not (vlax-erased-p current_temp_pline)))
          (vla-delete current_temp_pline)
        )
        nil
      )
    )
  )

  ;; --- 수정된 되돌리기 기능이 있는 텍스트 객체 선택 함수 ---
(defun get-valid-text-object-with-back (prompt_msg space_type / user_input obj_ename obj_vla min_pt max_pt text_center)
  (setq obj_vla nil)
  (while (not obj_vla)
    ;; 되돌리기 옵션 추가
    (initget "Back")
    (setq user_input (entsel (strcat prompt_msg " [Back: 되돌리기]: ")))
    
    (cond
      ;; 되돌리기 선택
      ((= user_input "Back")
       (setq obj_vla "BACK"))
      
      ;; 유효한 객체 선택
      ((and user_input (listp user_input))
       (setq obj_ename (car user_input))
       (if (and (member (cdr (assoc 0 (entget obj_ename))) '("TEXT" "MTEXT"))
                (if (= space_type "model")
                  (/= (cdr (assoc 67 (entget obj_ename))) 1)  ; 모형 공간
                  (= (cdr (assoc 67 (entget obj_ename))) 1)))  ; 배치 공간
         (progn
           (setq obj_vla (vlax-ename->vla-object obj_ename))
           
           ;; --- 텍스트 선택 시, 텍스트의 중심점을 화면 중심으로 저장 ---
           (vl-catch-all-apply
             '(lambda ()
                ;; 텍스트 객체의 경계상자(Bounding Box)를 가져옴
                (vla-getboundingbox obj_vla 'min_pt 'max_pt)
                (setq min_pt (vlax-safearray->list min_pt)
                      max_pt (vlax-safearray->list max_pt))
                
                ;; 경계상자의 두 대각점의 중간점을 계산하여 중심점을 구함
                (setq text_center 
                      (list (/ (+ (car min_pt) (car max_pt)) 2.0)
                            (/ (+ (cadr min_pt) (cadr max_pt)) 2.0)))
                
                ;; 계산된 중심점을 텍스트 복원용 변수에 저장
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
      
      ;; nil이나 기타 (빈 곳 클릭 등) - 모두 무시하고 계속
      (t 
       (prompt "\n>> TEXT 또는 MTEXT를 선택하거나 'Back'을 입력하세요."))
    )
  )
  obj_vla
)

  ;; --- 측정값을 화면에 표시하고 텍스트에 최종값을 입력하는 함수 ---
  (defun display-and-input-measurement (prompt_val text_val sel_obj_txt)
    (prompt "\n========================================")
    (prompt (strcat "\n  측정 결과: " prompt_val))
    (prompt "\n========================================")
    (vla-put-textstring sel_obj_txt text_val)
    (prompt (strcat "\n완료: '" text_val "' 값이 입력되었습니다.")))

  ;; --- 선택집합 해제 함수 ---
  (defun clear-selection-set ()
    (if g_sr_selection_set
      (progn
        (setq g_sr_selection_set nil) ; 선택집합 해제
        (sssetfirst nil nil) ; AutoCAD의 현재 선택집합 해제
        (vl-catch-all-apply '(lambda () (command-s "_.REDRAW"))) ; 화면 갱신
      )
    )
  )

  ;; --- 수정된 타입 호환성 체크 함수 ---
  (defun is-compatible-measurement (existing_type new_type current_mode / result)
    (setq result nil)
    (cond
      ;; 길이 합산 모드일 때 - 모든 길이 타입은 호환 가능
      ((= current_mode "LENGTH")
       (cond
         ;; 기존이 길이면 새로운 것도 길이여야 함
         ((= existing_type "L=")
          (if (= new_type "L=") (setq result t)))
         ;; 기존이 넓이면 길이 합산 모드에서는 길이로 변환되므로 호환
         ((= existing_type "A=")
          (if (= new_type "L=") (setq result t)))
         ;; 기존이 개수면 새로운 것도 개수여야 함
         ((= existing_type "")
          (if (= new_type "") (setq result t)))
         ;; 기타
         (t (setq result nil))
       )
      )
      ;; 일반 모드일 때
      (t
       (if (= existing_type new_type) (setq result t))
      )
    )
    result
  )

  ;; --- 수정된 기본 객체 선택 함수 (연속 측정 지원) ---
  (defun perform-object-selection (/ first_sel first_obj_vla first_obj_type manual_result temp_ename
                                     measurement_data_list total_value measurement_type
                                     continue_selection user_sel sel_obj_vla sel_obj_ename new_data prompt_string
                                     current_measurement_mode)
    ;; D 키워드 사용 설정
    (initget "D")
    (setq first_sel (entsel "\n측정할 첫번째 객체를 선택하세요 : / D : 수동측정"))

    (cond
      ;; "D"를 입력한 경우 수동 측정 함수 호출
      ((= first_sel "D")
       (setq manual_result (perform-manual-measurement))
       (if manual_result
         (progn
           ;; 첫 번째 수동 측정 결과
           (setq measurement_data_list (car manual_result)
                 total_value (cadr manual_result)
                 measurement_type (caar (car manual_result)))
           
           ;; 임시 객체 정보를 전역 변수 리스트에 추가
           (if (not g_temp_measurement_object_list)
             (setq g_temp_measurement_object_list '())
           )
           (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
           
           ;; 현재 측정 모드 결정
           (setq current_measurement_mode 
             (cond
               ((= measurement_type "L=") "LENGTH")
               ((= measurement_type "A=") "AREA")
               (t nil)
             )
           )
           
           ;; 연속 측정 시작
           (setq continue_selection t)
           (while continue_selection
             (setq prompt_string (generate-dynamic-prompt measurement_data_list total_value))
             
             ;; D 키워드 다시 설정
             (initget "D")
             (setq user_sel (entsel (strcat prompt_string " / D : 수동측정")))
             
             (cond
               ;; Enter키로 완료
               ((not user_sel)
                (setq continue_selection nil))
               
               ;; "D"를 다시 입력한 경우
               ((= user_sel "D")
                (setq manual_result (perform-manual-measurement))
                (if manual_result
                  (progn
                    (setq new_data (car manual_result))
                    
                    ;; 수정된 타입 체크 로직
                    (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
                      (progn
                        ;; 정상적으로 추가
                        (setq measurement_data_list (append measurement_data_list new_data)
                              total_value (+ total_value (cadr manual_result)))
                        
                        ;; 새 임시 객체를 리스트에 추가
                        (if (not g_temp_measurement_object_list)
                          (setq g_temp_measurement_object_list '())
                        )
                        (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
                      )
                      (progn
                        (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                        ;; 추가 생성된 임시 객체 삭제
                        (delete-temp-object (caddr manual_result))
                      )
                    )
                  )
                )
               )
               
               ;; 유효한 객체를 선택한 경우
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
           
           ;; 측정 데이터와 총합 반환
           (list measurement_data_list total_value)
         )
         nil
       )
      )

      ;; 객체를 선택한 경우 (기존 로직)
      ((listp first_sel)
        (setq first_obj_vla (vlax-ename->vla-object (car first_sel))
              first_obj_type (vla-get-objectname first_obj_vla))
        
        (if (= first_obj_type "AcDbBlockReference")
          (process-block-selection first_obj_vla)
          (process-object-selection-only first_obj_vla)
        )
      )
      
      ;; Enter나 취소시 nil 반환 (기존 로직)
      (t nil)
    )
  )

  ;; --- 뷰포트용 객체 선택 함수 (연속 측정 지원) ---
  (defun perform-object-selection-with-viewport-exit (acad_doc original_mspace_state / 
                                                      first_sel first_obj_vla first_obj_type manual_result
                                                      measurement_data_list total_value measurement_type
                                                      continue_selection user_sel sel_obj_vla sel_obj_ename new_data prompt_string
                                                      current_measurement_mode)
    ;; D 키워드 사용 설정
    (initget "D")
    (setq first_sel (entsel "\n측정할 첫번째 객체를 선택하세요 : / D : 수동측정"))

    (cond
      ;; "D"를 입력한 경우 수동 측정 함수 호출
      ((= first_sel "D")
       (setq manual_result (perform-manual-measurement))
       (if manual_result
         (progn
           ;; 첫 번째 수동 측정 결과
           (setq measurement_data_list (car manual_result)
                 total_value (cadr manual_result)
                 measurement_type (caar (car manual_result)))
           
           ;; 임시 객체 정보를 전역 변수 리스트에 추가
           (if (not g_temp_measurement_object_list)
             (setq g_temp_measurement_object_list '())
           )
           (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
           
           ;; 현재 측정 모드 결정
           (setq current_measurement_mode 
             (cond
               ((= measurement_type "L=") "LENGTH")
               ((= measurement_type "A=") "AREA")
               (t nil)
             )
           )
           
           ;; 연속 측정 로직 (ESC 처리 추가)
           (setq continue_selection t)
           (while continue_selection
             (setq prompt_string (generate-dynamic-prompt measurement_data_list total_value))
             
             (initget "D")
             (setq user_sel (entsel (strcat prompt_string " / D : 수동측정 (ESC: 취소)")))
             
             (cond
               ;; Enter키로 완료 또는 ESC로 취소
               ((not user_sel)
                (setq continue_selection nil))
               
               ;; "D"를 다시 입력한 경우 (위와 동일한 로직)
               ((= user_sel "D")
                (setq manual_result (perform-manual-measurement))
                (if manual_result
                  (progn
                    (setq new_data (car manual_result))
                    ;; 수정된 타입 체크 로직
                    (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
                      (progn
                        (setq measurement_data_list (append measurement_data_list new_data)
                              total_value (+ total_value (cadr manual_result)))
                        ;; 새 임시 객체를 리스트에 추가
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
               
               ;; 유효한 객체 선택 (위와 동일한 로직)
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
      
      ;; 객체를 선택한 경우 (기존 로직)
      ((listp first_sel)
        (setq first_obj_vla (vlax-ename->vla-object (car first_sel))
              first_obj_type (vla-get-objectname first_obj_vla))
        
        (if (= first_obj_type "AcDbBlockReference")
          (process-block-selection-with-viewport-exit first_obj_vla acad_doc original_mspace_state)
          (process-object-selection-only-with-viewport-exit first_obj_vla acad_doc original_mspace_state)
        )
      )
      
      ;; ESC나 취소시 배치공간으로 복귀 (기존 로직)
      (t
       (prompt "\n>> 취소")
       (vla-put-mspace acad_doc original_mspace_state)
       nil)
    )
  )

  ;; --- 수정된 객체 선택만 처리하는 함수 ---
  (defun process-object-selection-only (first_obj_vla / continue_selection user_sel sel_obj_vla sel_obj_ename new_data
                                        measurement_data_list total_value measurement_type
                                        prompt_string current_measurement_mode)
    (setq continue_selection t measurement_data_list '() total_value 0.0 measurement_type nil)
    
    ;; 첫 번째로 선택된 객체 먼저 처리
    (setq new_data (get-measurement-data first_obj_vla nil)) ; 첫 객체는 기본 모드
    (setq measurement_type (car new_data)
          measurement_data_list (list new_data)
          total_value (cadr new_data))
    
    ;; 현재 측정 모드 결정 (첫 객체 기준)
    (setq current_measurement_mode 
      (cond
        ((= (car new_data) "L=") "LENGTH")
        ((= (car new_data) "A=") "AREA")
        ((= (car new_data) "") "COUNT")
        (t nil)
      )
    )
    
    ;; 선택집합에 추가하여 강조 표시
    (add-to-selection-set first_obj_vla)
    
    (while continue_selection
      (setq prompt_string (generate-dynamic-prompt measurement_data_list total_value))
      ;; D 키워드 추가
      (initget "D")
      (setq user_sel (entsel (strcat prompt_string " / D : 수동측정")))
      
      (cond
        ;; Enter키나 nil을 반환했을 때
        ((not user_sel)
         (setq continue_selection nil))
        
        ;; "D"를 입력한 경우 수동 측정
        ((= user_sel "D")
         (setq manual_result (perform-manual-measurement))
         (if manual_result
           (progn
             (setq new_data (car manual_result))
             ;; 수정된 타입 체크 로직
             (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
               (progn
                 ;; 정상적으로 추가
                 (setq measurement_data_list (append measurement_data_list new_data)
                       total_value (+ total_value (cadr manual_result)))
                 
                 ;; 새 임시 객체를 리스트에 추가
                 (if (not g_temp_measurement_object_list)
                   (setq g_temp_measurement_object_list '())
                 )
                 (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
               )
               (progn
                 (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                 ;; 추가 생성된 임시 객체 삭제
                 (delete-temp-object (caddr manual_result))
               )
             )
           )
         )
        )
        
        ;; 유효한 객체를 선택했을 때
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

  ;; --- 뷰포트용 객체 선택 처리 함수 ---
  (defun process-object-selection-only-with-viewport-exit (first_obj_vla acad_doc original_mspace_state / 
                                                           continue_selection user_sel sel_obj_vla sel_obj_ename new_data
                                                           measurement_data_list total_value measurement_type
                                                           prompt_string current_measurement_mode manual_result)
    (setq continue_selection t measurement_data_list '() total_value 0.0 measurement_type nil)
    
    ;; 첫 번째로 선택된 객체 먼저 처리
    (setq new_data (get-measurement-data first_obj_vla nil))
    (setq measurement_type (car new_data)
          measurement_data_list (list new_data)
          total_value (cadr new_data))
    
    ;; 현재 측정 모드 결정 (첫 객체 기준)
    (setq current_measurement_mode 
      (cond
        ((= (car new_data) "L=") "LENGTH")
        ((= (car new_data) "A=") "AREA")
        ((= (car new_data) "") "COUNT")
        (t nil)
      )
    )
    
    ;; 선택집합에 추가하여 강조 표시
    (add-to-selection-set first_obj_vla)
    
    (while continue_selection
      (setq prompt_string (generate-dynamic-prompt measurement_data_list total_value))
      ;; D 키워드 추가
      (initget "D")
      (setq user_sel (entsel (strcat prompt_string " / D : 수동측정 (ESC: 취소)")))
      
      (cond
        ;; Enter키나 nil을 반환했을 때
        ((not user_sel)
         (setq continue_selection nil))
        
        ;; "D"를 입력한 경우 수동 측정
        ((= user_sel "D")
         (setq manual_result (perform-manual-measurement))
         (if manual_result
           (progn
             (setq new_data (car manual_result))
             ;; 수정된 타입 체크 로직
             (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
               (progn
                 ;; 정상적으로 추가
                 (setq measurement_data_list (append measurement_data_list new_data)
                       total_value (+ total_value (cadr manual_result)))
                 
                 ;; 새 임시 객체를 리스트에 추가
                 (if (not g_temp_measurement_object_list)
                   (setq g_temp_measurement_object_list '())
                 )
                 (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
               )
               (progn
                 (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                 ;; 추가 생성된 임시 객체 삭제
                 (delete-temp-object (caddr manual_result))
               )
             )
           )
         )
        )
        
        ;; 유효한 객체를 선택했을 때
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
    
    ;; 정상 완료된 경우 결과 반환
    (if (and measurement_data_list (> total_value 0))
      (list measurement_data_list total_value)
      nil
    )
  )

  ;; --- 블록 선택 및 카운트 전용 함수 ---
  (defun process-block-selection (first_block_vla / blk_name prompt_msg ss i ename obj_vla
                                                   measurement_data_list total_value)
    (setq blk_name (vla-get-effectivename first_block_vla))
    (setq prompt_msg (strcat "\n'" blk_name "' 블록을 모두 선택하세요. 첫번째 블록은 이미 포함되었습니다: "))
    
    ;; 프롬프트 출력
    (prompt prompt_msg)
    
    ;; ssget 호출
    (setq ss (ssget (list (cons 0 "INSERT") (cons 2 blk_name))))

    (setq measurement_data_list '() total_value 0.0)

    ;; 첫 번째 선택된 블록을 먼저 리스트에 추가
    (setq measurement_data_list (list (list "" 1 "ea"))
          total_value 1.0)
    
    ;; 선택집합에 추가하여 강조 표시
    (add-to-selection-set first_block_vla)

    (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq ename (ssname ss i)
                obj_vla (vlax-ename->vla-object ename))
          ;; 첫번째 객체와 중복 선택 방지
          (if (/= (vla-get-handle obj_vla) (vla-get-handle first_block_vla))
            (progn
               (setq measurement_data_list (append measurement_data_list (list (list "" 1 "ea")))
                    total_value (1+ total_value))
              
              ;; 선택집합에 추가하여 강조 표시
              (add-to-selection-set obj_vla)
            )
          )
          (setq i (1+ i))
        )
      )
    )
    (list measurement_data_list total_value)
  )

  ;; --- 뷰포트용 블록 선택 및 카운트 전용 함수 ---
  (defun process-block-selection-with-viewport-exit (first_block_vla acad_doc original_mspace_state / 
                                                     blk_name prompt_msg ss i ename obj_vla
                                                     measurement_data_list total_value)
    (setq blk_name (vla-get-effectivename first_block_vla))
    (setq prompt_msg (strcat "\n'" blk_name "' 블록을 모두 선택하세요. 첫번째 블록은 이미 포함되었습니다 (ESC: 취소): "))
    
    ;; 프롬프트 출력
    (prompt prompt_msg)
    
    ;; ssget 호출
    (setq ss (ssget (list (cons 0 "INSERT") (cons 2 blk_name))))

    ;; ESC로 취소된 경우 처리
    (if (not ss)
      (progn
        (prompt "\n>> 취소")
        (clear-selection-set)
        (vla-put-mspace acad_doc original_mspace_state)
        nil)
      (progn
        (setq measurement_data_list '() total_value 0.0)

        ;; 첫 번째 선택된 블록을 먼저 리스트에 추가
        (setq measurement_data_list (list (list "" 1 "ea"))
              total_value 1.0)
        
        ;; 선택집합에 추가하여 강조 표시
        (add-to-selection-set first_block_vla)

        (setq i 0)
        (while (< i (sslength ss))
          (setq ename (ssname ss i)
                obj_vla (vlax-ename->vla-object ename))
          ;; 첫번째 객체와 중복 선택 방지
          (if (/= (vla-get-handle obj_vla) (vla-get-handle first_block_vla))
            (progn
              (setq measurement_data_list (append measurement_data_list (list (list "" 1 "ea")))
                    total_value (1+ total_value))
              
              ;; 선택집합에 추가하여 강조 표시
              (add-to-selection-set obj_vla)
            )
          )
          (setq i (1+ i))
        )
        
        (list measurement_data_list total_value)
      )
    )
  )

  ;; --- 추가 객체 선택을 위한 보조 함수 ---
  (defun perform-additional-object-selection (existing_data_list existing_total existing_type /
                                             continue_selection user_sel sel_obj_vla sel_obj_ename new_data
                                             measurement_data_list total_value measurement_type
                                             prompt_string current_measurement_mode manual_result)
    (setq continue_selection t 
          measurement_data_list existing_data_list 
          total_value existing_total 
          measurement_type existing_type)
    
    ;; 현재 측정 모드 결정
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
      ;; D 키워드 추가
      (initget "D")
      (setq user_sel (entsel (strcat prompt_string " / D : 수동측정")))
      
      (cond
        ;; Enter로 완료
        ((not user_sel)
         (setq continue_selection nil))
        
        ;; "D"를 입력한 경우 수동 측정
        ((= user_sel "D")
         (setq manual_result (perform-manual-measurement))
         (if manual_result
           (progn
             (setq new_data (car manual_result))
             ;; 수정된 타입 체크 로직
             (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
               (progn
                 ;; 정상적으로 추가
                 (setq measurement_data_list (append measurement_data_list new_data)
                       total_value (+ total_value (cadr manual_result)))
                 
                 ;; 새 임시 객체를 리스트에 추가
                 (if (not g_temp_measurement_object_list)
                   (setq g_temp_measurement_object_list '())
                 )
                 (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
               )
               (progn
                 (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                 ;; 추가 생성된 임시 객체 삭제
                 (delete-temp-object (caddr manual_result))
               )
             )
           )
         )
        )
        
        ;; 유효한 객체 선택
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

  ;; --- 뷰포트용 추가 객체 선택 함수 ---
  (defun perform-additional-object-selection-with-viewport-exit (existing_data_list existing_total existing_type 
                                                                acad_doc original_mspace_state /
                                                                continue_selection user_sel sel_obj_vla sel_obj_ename new_data
                                                                measurement_data_list total_value measurement_type
                                                                prompt_string current_measurement_mode manual_result)
    (setq continue_selection t 
          measurement_data_list existing_data_list 
          total_value existing_total 
          measurement_type existing_type)
    
    ;; 현재 측정 모드 결정
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
      ;; D 키워드 추가
      (initget "D")
      (setq user_sel (entsel (strcat prompt_string " / D : 수동측정 (ESC: 취소)")))
      
      (cond
        ;; Enter로 완료 또는 ESC로 취소
        ((not user_sel)
         (setq continue_selection nil))
        
        ;; "D"를 입력한 경우 수동 측정
        ((= user_sel "D")
         (setq manual_result (perform-manual-measurement))
         (if manual_result
           (progn
             (setq new_data (car manual_result))
             ;; 수정된 타입 체크 로직
             (if (is-compatible-measurement measurement_type (caar new_data) current_measurement_mode)
               (progn
                 ;; 정상적으로 추가
                 (setq measurement_data_list (append measurement_data_list new_data)
                       total_value (+ total_value (cadr manual_result)))
                 
                 ;; 새 임시 객체를 리스트에 추가
                 (if (not g_temp_measurement_object_list)
                   (setq g_temp_measurement_object_list '())
                 )
                 (setq g_temp_measurement_object_list (append g_temp_measurement_object_list (list (caddr manual_result))))
               )
               (progn
                 (prompt "\n>> 오류: 측정 타입이 일치하지 않습니다.")
                 ;; 추가 생성된 임시 객체 삭제
                 (delete-temp-object (caddr manual_result))
               )
             )
           )
         )
        )
        
        ;; 유효한 객체 선택
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
    
    ;; 결과 반환
    (if (and measurement_data_list (> total_value 0))
      (list measurement_data_list total_value)
      nil
    )
  )

  ;; --- 수정된 되돌리기를 지원하는 메인 측정 함수 ---
  (defun process-multi-selection-with-back (space_type first_run / 
                                            selection_result measurement_data_list total_value
                                            final_data_list prompt_string final_text_string sel_obj_txt text_prompt
                                            measurement_complete additional_result)
    
    ;; 첫 실행이 아닌 경우 이전 화면으로 복원 시도
    (prompt (strcat "\n[DEBUG] 함수 시작 - space_type: " space_type ", first_run: " (vl-princ-to-string first_run)))
    
    (if (not first_run)
      (progn
        (prompt "\n[DEBUG] 조건 만족 - 화면 복원 시도")
        (if (= space_type "model")
          (if (and g_saved_view_center g_viewport_height)
            (progn
              (prompt "\n>> 이전 측정 위치로 화면을 복원합니다...")
              (restore-saved-view)
            )
            (prompt "\n[DEBUG] 저장된 모델공간 화면 정보가 없어서 복원 불가")
          )
          ;; 배치공간
          (if (and g_saved_paper_view_center g_paper_viewport_height)
            (progn
              (prompt "\n>> 이전 측정 위치로 화면을 복원합니다...")
              (restore-saved-paper-view)
            )
            (prompt "\n[DEBUG] 저장된 배치공간 화면 정보가 없어서 복원 불가")
          )
        )
      )
      (prompt "\n[DEBUG] 첫 실행 - 화면 복원 생략")
    )
    
    ;; 1단계: 초기 객체 선택
    (setq selection_result (perform-object-selection))
    
    (if selection_result
      (progn
        (setq measurement_data_list (car selection_result)
              total_value (cadr selection_result))
        
        (setq measurement_complete nil)
        (while (not measurement_complete)
          
          ;; 현재 측정 데이터로 텍스트 준비
          (setq final_data_list (list (caar measurement_data_list) total_value (caddar measurement_data_list))
                final_text_string (format-rounded-string final_data_list))
          
          (setq text_prompt (strcat "\n측정값 '" 
                                   (car final_data_list) 
                                   (rtos (cadr final_data_list) 2 2) 
                                   (caddr final_data_list) 
                                   "' 을 입력할 텍스트를 선택하세요"))
          
          ;; --- 측정 완료 후 텍스트 선택 프롬프트 직전에 화면 상태 저장 ---
          (if (= space_type "model")
            (progn
              (prompt "\n[DEBUG] 모형공간 모드 - 화면 저장 함수 호출")
              (save-current-view)
            )
            (progn
              (prompt "\n[DEBUG] 배치공간 모드 - 화면 저장 함수 호출")
              (save-current-paper-view)
            )
          )
          
          ;; 2단계: 텍스트 선택 (이전 텍스트 위치로 복원 후)
          (if (not first_run)
            (if (= space_type "model")
              (if g_saved_text_center
                (progn
                  (prompt "\n>> 이전 텍스트 위치로 화면을 복원합니다...")
                  (setq text_center_3d (list (car g_saved_text_center) (cadr g_saved_text_center) 0.0))
                  (vl-catch-all-apply '(lambda () (command-s "_.zoom" "_c" text_center_3d g_viewport_height)))
                )
              )
              ;; 배치공간
              (if g_saved_paper_text_center
                (progn
                  (prompt "\n>> 이전 텍스트 위치로 화면을 복원합니다...")
                  (setq text_center_3d (list (car g_saved_paper_text_center) (cadr g_saved_paper_text_center) 0.0))
                  (vl-catch-all-apply '(lambda () (command-s "_.zoom" "_c" text_center_3d g_paper_viewport_height)))
                )
              )
            )
          )
          (setq sel_obj_txt (get-valid-text-object-with-back text_prompt space_type))
          
          (cond
            ;; 되돌리기 - 추가 객체 선택 모드로
            ((= sel_obj_txt "BACK")
             (prompt "\n>> 되돌리기: 추가 객체를 선택할 수 있습니다. (현재 측정값 보존)")
             
             ;; 추가 객체 선택
             (setq additional_result (perform-additional-object-selection 
                                      measurement_data_list 
                                      total_value 
                                      (caar measurement_data_list)))
             
             ;; 추가 선택 결과 업데이트
             (if additional_result
               (progn
                 (setq measurement_data_list (car additional_result)
                       total_value (cadr additional_result))
                 (prompt "\n>> 추가 객체 선택이 완료되었습니다.")
               )
             )
             ;; 다시 텍스트 선택으로
             )
            
            ;; 정상 텍스트 선택
            ((and sel_obj_txt (/= sel_obj_txt "BACK"))
             (setq prompt_string (strcat (car final_data_list) (rtos (cadr final_data_list) 2 2) (caddr final_data_list)))
             (display-and-input-measurement prompt_string final_text_string sel_obj_txt)
             (setq measurement_complete t))  ; 완료
            
            ;; 취소
            (t 
             (prompt "\n*텍스트 선택이 취소되었습니다.*")
             ;; ★★★ 레이어 상태 복원 (텍스트 선택 취소 시) ★★★
             (if g_layer_separation_active
               (restore-layer-states)
             )
             (setq measurement_complete t))  ; 종료
          )
        )
      )
      (progn
        ;; 객체 선택이 취소된 경우
        (prompt "\n*객체 선택이 취소되었습니다.*")
        ;; ★★★ 레이어 상태 복원 (객체 선택 취소 시) ★★★
        (if g_layer_separation_active
          (restore-layer-states)
        )
      )
    )
    
    ;; 선택집합 해제
    (clear-selection-set)
    
    ;; --- 임시 측정 객체 삭제 ---
    (delete-all-temp-objects)
  )

  ;; --- ★★★ 수정된 모형공간 반복 측정 함수 (레이어 분리 기능 + 화면 위치 기억) ★★★
  (defun perform-modelspace-measurements (/ continue_work user_choice first_run layer_separation_choice)
    (setq continue_work t first_run t)
    
    (while continue_work
      (prompt "\n----------------------------------------")
      
      ;; ★★★ 레이어 분리 기능 추가 (모형 공간용) ★★★
      (if first_run
        (progn
          (initget "Yes No")
          (setq layer_separation_choice (getkword "\n레이어 분리를 하시겠습니까? [예(Y)/아니오(N)] <아니오>: "))
          (if (not layer_separation_choice) (setq layer_separation_choice "No"))
          
          (if (= layer_separation_choice "Yes")
            (progn
              (prompt "\n=== 모형 공간 레이어 분리 시작 (외부참조 지원) ===")
              (if (perform-layer-separation)
                (prompt "\n>> 레이어 분리가 완료되었습니다. 객체를 선택하세요.")
                (prompt "\n>> 레이어 분리를 건너뜁니다.")
              )
            )
            (prompt "\n>> 레이어 분리를 건너뜁니다.")
          )
        )
      )
      
      (process-multi-selection-with-back "model" first_run)
      
      (setq first_run nil) ; 첫 실행 이후로 설정
      
      (initget "Yes No")
      (setq user_choice (getkword "\n다른 객체를 측정하시겠습니까? [예(Y)/아니오(N)] <예>: "))
      (if (not user_choice) (setq user_choice "Yes"))
      
      ;; ★★★ 연속 측정 종료 시에만 레이어 상태 복원 ★★★
      (if (= user_choice "No") 
        (progn
          (setq continue_work nil)
          (if g_layer_separation_active
            (progn
              (prompt "\n>> 모형 공간 측정 작업 완료. 레이어 상태를 복원합니다...")
              (restore-layer-states)
            )
          )
        )
        ;; 연속 측정 시에는 레이어 분리 상태 유지
        (if g_layer_separation_active
          (prompt "\n>> 레이어 분리 상태를 유지합니다. 다음 객체를 선택하세요.")
        )
      )
    )
  )

  ;; --- ★★★ 수정된 배치공간 반복 측정 함수 (레이어 분리 기능 + 화면 위치 기억) ★★★
  (defun perform-paperspace-measurements (/ continue_work user_choice first_run layer_separation_choice)
    (setq continue_work t first_run t)
    
    (while continue_work
      (prompt "\n----------------------------------------")
      
      ;; ★★★ 레이어 분리 기능 추가 (배치 공간용) ★★★
      (if first_run
        (progn
          (initget "Yes No")
          (setq layer_separation_choice (getkword "\n레이어 분리를 하시겠습니까? [예(Y)/아니오(N)] <아니오>: "))
          (if (not layer_separation_choice) (setq layer_separation_choice "No"))
          
          (if (= layer_separation_choice "Yes")
            (progn
              (prompt "\n=== 배치 공간 레이어 분리 시작 (외부참조 지원) ===")
              (if (perform-layer-separation)
                (prompt "\n>> 레이어 분리가 완료되었습니다. 객체를 선택하세요.")
                (prompt "\n>> 레이어 분리를 건너뜁니다.")
              )
            )
            (prompt "\n>> 레이어 분리를 건너뜁니다.")
          )
        )
      )
      
      (process-multi-selection-with-back "paper" first_run)
      
      (setq first_run nil) ; 첫 실행 이후로 설정
      
      (initget "Yes No")
      (setq user_choice (getkword "\n다른 객체를 측정하시겠습니까? [예(Y)/아니오(N)] <예>: "))
      (if (not user_choice) (setq user_choice "Yes"))
      
      ;; ★★★ 연속 측정 종료 시에만 레이어 상태 복원 ★★★
      (if (= user_choice "No") 
        (progn
          (setq continue_work nil)
          (if g_layer_separation_active
            (progn
              (prompt "\n>> 배치 공간 측정 작업 완료. 레이어 상태를 복원합니다...")
              (restore-layer-states)
            )
          )
        )
        ;; 연속 측정 시에는 레이어 분리 상태 유지
        (if g_layer_separation_active
          (prompt "\n>> 레이어 분리 상태를 유지합니다. 다음 객체를 선택하세요.")
        )
      )
    )
  )

  ;; --- ★★★ 수정된 뷰포트 측정 함수 (레이어 분리 기능 + 외부참조 지원 추가) ---
(defun perform-viewport-measurements (vp_obj / continue_work user_choice original_mspace_state selection_result
                                              measurement_data_list total_value final_data_list
                                              prompt_string final_text_string sel_obj_txt text_selection_prompt
                                              measurement_complete additional_result first_run layer_separation_choice)
  
  (setq continue_work t first_run t)
  (while continue_work
    (prompt "\n----------------------------------------")
    
    ;; --- 첫 실행이 아닐 때 이전 화면으로 복원 ---
    (if (not first_run)
      (progn
        (prompt "\n>> 이전 측정 위치로 화면을 복원합니다...")
        (restore-viewport-paper-view)
      )
      ;; --- 첫 실행일 때만 뷰포트 전체로 줌 ---
      (progn
        (prompt "\n>> 뷰포트 화면으로 줌하여 활성화를 준비합니다...")
        (zoom-to-viewport vp_obj)
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
        
        ;; ★★★ 레이어 분리 기능 추가 (외부참조 지원) ★★★
        (if first_run
          (progn
            (initget "Yes No")
            (setq layer_separation_choice (getkword "\n레이어 분리를 하시겠습니까? [예(Y)/아니오(N)] <아니오>: "))
            (if (not layer_separation_choice) (setq layer_separation_choice "No"))
            
            (if (= layer_separation_choice "Yes")
              (progn
                (prompt "\n=== 레이어 분리 시작 (외부참조 지원) ===")
                (if (perform-layer-separation)
                  (prompt "\n>> 레이어 분리가 완료되었습니다. 객체를 선택하세요.")
                  (prompt "\n>> 레이어 분리를 건너뜁니다.")
                )
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
              
              ;; --- 텍스트 선택 직전, 현재 화면 상태 저장 ---
              (save-viewport-paper-view)

              ;; 이전 텍스트 위치로 복원 (첫 실행이 아닐 때)
              (if (and (not first_run) g_saved_paper_text_center)
                (progn
                  (prompt "\n>> 이전 텍스트 위치로 화면을 복원합니다...")
                  (setq text_center_3d (list (car g_saved_paper_text_center) (cadr g_saved_paper_text_center) 0.0))
                  (vl-catch-all-apply '(lambda () (command-s "_.zoom" "_c" text_center_3d g_paper_viewport_height)))
                )
              )
              
              (setq sel_obj_txt (get-valid-text-object-with-back text_selection_prompt "paper"))
              
              (cond
                ((= sel_obj_txt "BACK")
                 (prompt "\n>> 되돌리기: 추가 객체를 선택할 수 있습니다. (현재 측정값 보존)")
                 
                 ;; 뷰포트 재활성화 - 이전 측정 위치로 복원
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
                 ;; ★★★ 레이어 상태 복원 (텍스트 선택 취소 시) ★★★
                 (if g_layer_separation_active
                   (restore-layer-states)
                 )
                 (setq measurement_complete t))
              )
            )
          )
          (progn
            (prompt "\n*객체 선택이 취소되었습니다.*")
            ;; ★★★ 레이어 상태 복원 (객체 선택 취소 시) ★★★
            (if g_layer_separation_active
              (restore-layer-states)
            )
          )
        )
        
        (clear-selection-set)
        
        ;; --- 임시 측정 객체들 삭제 ---
        (delete-all-temp-objects)
        
        (setq first_run nil)

        (initget "Yes No")
        (setq user_choice (getkword "\n같은 뷰포트에서 다른 객체를 측정하시겠습니까? [예(Y)/아니오(N)] <예>: "))
        (if (not user_choice) (setq user_choice "Yes"))
        
        ;; ★★★ 연속 측정 종료 시에만 레이어 상태 복원 ★★★
        (if (= user_choice "No") 
          (progn
            (setq continue_work nil)
            (if g_layer_separation_active
              (progn
                (prompt "\n>> 측정 작업 완료. 레이어 상태를 복원합니다...")
                (restore-layer-states)
              )
            )
          )
          ;; 연속 측정 시에는 레이어 분리 상태 유지
          (if g_layer_separation_active
            (prompt "\n>> 레이어 분리 상태를 유지합니다. 다음 객체를 선택하세요.")
          )
        )
      )
    )
  )
)

  ;; --- 메인 프로그램 시작 ---
  (setq old_error *error* *error* sr_error_handler) ; 안전장치 활성화

  (if (= (getvar "TILEMODE") 1)
    (progn
      (prompt "\n[모형 공간 측정 모드 - 레이어 분리 지원]")
      (perform-modelspace-measurements)
      (prompt "\n>> 모형 공간 측정 작업이 완료되었습니다.")
    )
    (progn
      (initget "Viewport Paperspace")
      (setq mode (getkword "\n작업을 선택하세요 [뷰포트 내부(V)/배치 공간(P)] <뷰포트 내부>: "))
      (if (not mode) (setq mode "Viewport"))
      
      (if (= mode "Viewport")
        (progn
          (prompt "\n[뷰포트 내부 측정 모드 - 레이어 분리 지원]")
          (prompt "\n** 안내: 뷰포트나 폴리선 경계만 선택하세요. **")
          ;; 뷰포트 선택
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
              ;; user_input이 nil인 경우 (빈칸 클릭) 무시하고 계속
            )
          )
          
          (perform-viewport-measurements target_vp_obj)
          (prompt "\n>> 뷰포트 내부 측정 작업이 완료되었습니다.")
        )
      )
      
      (if (= mode "Paperspace")
        (progn
          (prompt "\n[배치 공간 직접 측정 모드 - 레이어 분리 지원]")
          (perform-paperspace-measurements)
          (prompt "\n>> 배치 공간 측정 작업이 완료되었습니다.")
        )
      )
    )
  )

  ;; 프로그램 종료 전 최종 정리
  (if g_layer_separation_active
    (restore-layer-states)
  )

  (setq *error* old_error)
  (prompt "\n========================================")
  (prompt "\n  SR 명령이 완료되었습니다.")
  (prompt "\n========================================")
  (princ)
)

;; 프로그램 로드 완료 메시지
(prompt "\n'SR' 명령이 로드되었습니다. (모든 측정 모드에서 레이어 분리 + 외부참조 완전 지원)")
(princ)