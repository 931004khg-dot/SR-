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