;; 간단한 XREF 블록 감지 테스트
(defun C:XTEST (/ sel obj_vla blocks_collection block_def)
  (vl-load-com)
  
  (prompt "\n=== 간단 XREF 테스트 ===")
  
  (if (setq sel (ssget ":S"))  ; 단일 객체 선택
    (progn
      (setq obj_vla (vlax-ename->vla-object (ssname sel 0)))
      
      (prompt (strcat "\n객체 타입: " (vla-get-objectname obj_vla)))
      (prompt (strcat "\n레이어: " (vla-get-layer obj_vla)))
      
      (if (= (vla-get-objectname obj_vla) "AcDbBlockReference")
        (progn
          (setq block_name (vla-get-name obj_vla))
          (prompt (strcat "\n블록 이름: " block_name))
          
          (setq blocks_collection (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))))
          (setq block_def (vla-item blocks_collection block_name))
          
          (if (= (vla-get-isxref block_def) :vlax-true)
            (prompt "\n★ 이것은 XREF 블록입니다!")
            (prompt "\n- 이것은 일반 블록입니다.")
          )
        )
        (prompt "\n- 블록이 아닙니다.")
      )
    )
    (prompt "\n객체를 선택하지 않았습니다.")
  )
  
  (princ)
)

(prompt "\nXTEST 명령이 로드되었습니다.")
(princ)