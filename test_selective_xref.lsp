;; Test script for selective XREF layer expansion
;; This demonstrates the improved layer separation logic

(defun is-xref-layer (layer_name)
  (if (vl-string-search "|" layer_name) t nil)
)

(defun test-selective-xref-logic ()
  (princ "\n=== Testing Selective XREF Layer Expansion ===")
  
  ;; Test case 1: Regular layers only (no XREF)
  (setq test_layers_1 '("Layer1" "Layer2" "Dimensions"))
  (setq has_xref_1 nil)
  (foreach layer_name test_layers_1
    (if (is-xref-layer layer_name)
      (setq has_xref_1 t)
    )
  )
  (princ (strcat "\nTest 1 - Regular layers: " (vl-princ-to-string test_layers_1)))
  (princ (strcat "\n  Has XREF selection: " (if has_xref_1 "YES" "NO")))
  (princ (if has_xref_1 "\n  → XREF expansion WILL occur" "\n  → XREF expansion SKIPPED"))
  
  ;; Test case 2: Mixed layers (includes XREF)
  (setq test_layers_2 '("Layer1" "Building|Walls" "Dimensions"))
  (setq has_xref_2 nil)
  (foreach layer_name test_layers_2
    (if (is-xref-layer layer_name)
      (setq has_xref_2 t)
    )
  )
  (princ (strcat "\n\nTest 2 - Mixed layers: " (vl-princ-to-string test_layers_2)))
  (princ (strcat "\n  Has XREF selection: " (if has_xref_2 "YES" "NO")))
  (princ (if has_xref_2 "\n  → XREF expansion WILL occur" "\n  → XREF expansion SKIPPED"))
  
  ;; Test case 3: All XREF layers
  (setq test_layers_3 '("Building|Walls" "Site|Dimensions" "Mech|Equipment"))
  (setq has_xref_3 nil)
  (foreach layer_name test_layers_3
    (if (is-xref-layer layer_name)
      (setq has_xref_3 t)
    )
  )
  (princ (strcat "\n\nTest 3 - All XREF layers: " (vl-princ-to-string test_layers_3)))
  (princ (strcat "\n  Has XREF selection: " (if has_xref_3 "YES" "NO")))
  (princ (if has_xref_3 "\n  → XREF expansion WILL occur" "\n  → XREF expansion SKIPPED"))
  
  (princ "\n\n=== Test Summary ===")
  (princ "\nBEFORE: All cases would trigger XREF expansion (unwanted behavior)")
  (princ "\nAFTER: Only cases 2 & 3 trigger XREF expansion (desired behavior)")
  (princ "\n=====================")
  (princ)
)

;; Run the test
(test-selective-xref-logic)