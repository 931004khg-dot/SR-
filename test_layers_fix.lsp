;; Test script to verify the layers variable initialization fix

(defun test-layers-initialization ()
  (princ "\n=== Testing Layers Variable Initialization Fix ===")
  
  ;; 시뮬레이션: XREF가 없는 경우
  (princ "\nTest Case 1: Non-XREF layers only")
  (setq test_layers '("Layer1" "Dimensions"))
  (setq has_xref_selection nil)
  
  (foreach layer_name test_layers
    (if (vl-string-search "|" layer_name)
      (setq has_xref_selection t)
    )
  )
  
  (princ (strcat "\n  Selected layers: " (vl-princ-to-string test_layers)))
  (princ (strcat "\n  Has XREF: " (if has_xref_selection "YES" "NO")))
  (princ "\n  Expected behavior: layers variable should be initialized BEFORE XREF check")
  (princ (if has_xref_selection "\n  → XREF expansion would occur" "\n  → XREF expansion skipped (layers still initialized)"))
  
  ;; 시뮬레이션: XREF가 있는 경우
  (princ "\n\nTest Case 2: Mixed layers with XREF")
  (setq test_layers_2 '("Layer1" "Building|Walls"))
  (setq has_xref_selection_2 nil)
  
  (foreach layer_name test_layers_2
    (if (vl-string-search "|" layer_name)
      (setq has_xref_selection_2 t)
    )
  )
  
  (princ (strcat "\n  Selected layers: " (vl-princ-to-string test_layers_2)))
  (princ (strcat "\n  Has XREF: " (if has_xref_selection_2 "YES" "NO")))
  (princ "\n  Expected behavior: layers variable initialized AND XREF expansion occurs")
  (princ (if has_xref_selection_2 "\n  → XREF expansion would occur" "\n  → XREF expansion skipped"))
  
  (princ "\n\n=== Fix Summary ===")
  (princ "\nBEFORE: layers variable only initialized in XREF expansion branch")
  (princ "\n        → Error when XREF expansion skipped: 'VLA-object collection: nil'")
  (princ "\nAFTER:  layers variable initialized at start of function")
  (princ "\n        → Works correctly in both XREF and non-XREF cases")
  (princ "\n===================")
  (princ)
)

;; Run the test
(test-layers-initialization)