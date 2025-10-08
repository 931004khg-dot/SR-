;; Test script for XREF block detection in layer separation

(defun test-xref-block-logic ()
  (princ "\n=== Testing XREF Block Detection Logic ===")
  
  ;; Simulate selected XREF blocks
  (setq selected_xref_blocks '("XR-현황" "XR-배치"))
  (setq all_layer_names '(
    "0" 
    "POINT_HEIGHT" 
    "XR-현황|Walls" 
    "XR-현황|Doors" 
    "XR-현황|Windows"
    "XR-배치|Structure"
    "XR-배치|Foundation"
    "Regular_Layer"
  ))
  
  (princ (strcat "\nSelected XREF blocks: " (vl-princ-to-string selected_xref_blocks)))
  (princ (strcat "\nAll available layers: " (vl-princ-to-string all_layer_names)))
  
  ;; Test XREF layer expansion for selected blocks
  (setq xref_layer_names '())
  (if selected_xref_blocks
    (foreach xref_block_name selected_xref_blocks
      (princ (strcat "\n\nProcessing XREF block: " xref_block_name))
      (foreach all_layer all_layer_names
        ;; Check if layer belongs to this XREF block
        (if (and (vl-string-search "|" all_layer)
                 (= (vl-string-search (strcat xref_block_name "|") all_layer) 0))
          (if (not (member all_layer xref_layer_names))
            (progn
              (setq xref_layer_names (cons all_layer xref_layer_names))
              (princ (strcat "\n  → Added XREF layer: " all_layer))
            )
          )
        )
      )
    )
  )
  
  (princ (strcat "\n\nFinal XREF layers to expand: " (vl-princ-to-string xref_layer_names)))
  
  ;; Test results
  (princ "\n\n=== Test Results ===")
  (princ (strcat "\nExpected layers for XR-현황: XR-현황|Walls, XR-현황|Doors, XR-현황|Windows"))
  (princ (strcat "\nExpected layers for XR-배치: XR-배치|Structure, XR-배치|Foundation"))
  (princ (strcat "\nActual result: " (vl-princ-to-string xref_layer_names)))
  
  (if (and (member "XR-현황|Walls" xref_layer_names)
           (member "XR-현황|Doors" xref_layer_names)
           (member "XR-배치|Structure" xref_layer_names))
    (princ "\n✅ XREF block detection working correctly!")
    (princ "\n❌ XREF block detection needs adjustment")
  )
  
  (princ "\n====================")
  (princ)
)

;; Run the test
(test-xref-block-logic)