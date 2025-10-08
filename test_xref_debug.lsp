;; XREF 블록 감지 테스트 및 디버깅 도구

(defun test-xref-detection (/ sel_set i obj_vla object_type block_name)
  (prompt "\n=== XREF 블록 감지 테스트 ===")
  (prompt "\n객체를 선택하세요:")
  
  (if (setq sel_set (ssget))
    (progn
      (setq i 0)
      (repeat (sslength sel_set)
        (setq obj_vla (vlax-ename->vla-object (ssname sel_set i))
              object_type (vla-get-objectname obj_vla))
              
        (prompt (strcat "\n--- 객체 " (rtos (1+ i) 2 0) " ---"))
        (prompt (strcat "\n  타입: " object_type))
        (prompt (strcat "\n  레이어: " (vla-get-layer obj_vla)))
        
        (if (= object_type "AcDbBlockReference")
          (progn
            (setq block_name (vla-get-name obj_vla))
            (prompt (strcat "\n  블록 이름: " block_name))
            
            ;; XREF 여부 확인
            (setq blocks_collection (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))))
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list blocks_collection block_name))))
              (progn
                (setq block_def (vla-item blocks_collection block_name))
                (setq is_xref (vla-get-isxref block_def))
                (prompt (strcat "\n  IsXref: " (vl-princ-to-string is_xref)))
                
                (if (= is_xref :vlax-true)
                  (prompt "\n  ★ 이 블록은 XREF입니다!")
                  (prompt "\n  - 이 블록은 일반 블록입니다.")
                )
              )
              (prompt "\n  [오류] 블록 정의를 찾을 수 없습니다.")
            )
          )
          (prompt "\n  - 블록 참조가 아닙니다.")
        )
        
        (setq i (1+ i))
      )
    )
    (prompt "\n선택된 객체가 없습니다.")
  )
  
  (prompt "\n=== 테스트 완료 ===")
  (princ)
)

;; 사용법: (test-xref-detection)
(prompt "\n테스트를 실행하려면 (test-xref-detection)를 입력하세요.")