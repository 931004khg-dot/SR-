# XREF 선택적 확장 수정사항

## 문제 상황
사용자가 레이어 분리 시 외부참조 객체를 선택하지 않았음에도 불구하고 XREF 객체들이 자동으로 보이는 현상이 발생했습니다.

### 기존 동작 (문제)
- 일반 레이어 객체만 선택해도 모든 관련 XREF 레이어가 자동으로 확장됨
- 사용자 의도와 다른 불필요한 XREF 객체 표시

## 해결 방안

### 핵심 수정 내용
1. **선택적 XREF 확장**: 사용자가 실제로 XREF 객체를 선택한 경우에만 XREF 레이어 확장 수행
2. **XREF 검색 로직**: 선택된 객체 중 XREF 레이어(`|` 포함)가 있는지 먼저 확인
3. **조건부 처리**: XREF 선택이 없으면 확장 과정을 완전히 건너뜀

### 수정된 파일
- `SR_with_xref_layer_support.lsp` (perform-layer-separation 함수)

### 추가된 함수
```lisp
;; 외부참조 레이어 확인 함수
(defun is-xref-layer (layer_name)
  (if (vl-string-search "|" layer_name) t nil)
)
```

### 수정된 로직
```lisp
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
    ;; XREF 확장 로직 실행...
  )
  (progn
    (prompt "\n>> 일반 레이어만 선택되어 XREF 확장을 건너뜁니다...")
    ;; 외부참조 선택이 없으면 원래 레이어만 사용
  )
)
```

## 테스트 케이스

### Case 1: 일반 레이어만 선택
- **입력**: ["Layer1", "Layer2", "Dimensions"]
- **결과**: XREF 확장 건너뜀 ✅
- **메시지**: "일반 레이어만 선택되어 XREF 확장을 건너뜁니다..."

### Case 2: 혼합 레이어 (XREF 포함)
- **입력**: ["Layer1", "Building|Walls", "Dimensions"]
- **결과**: XREF 확장 수행 ✅
- **메시지**: "외부참조 객체가 선택되어 관련 XREF 레이어를 확장합니다..."

### Case 3: 모든 XREF 레이어
- **입력**: ["Building|Walls", "Site|Dimensions", "Mech|Equipment"]
- **결과**: XREF 확장 수행 ✅
- **메시지**: "외부참조 객체가 선택되어 관련 XREF 레이어를 확장합니다..."

## 기대 효과
1. **정확한 레이어 분리**: 사용자가 선택한 객체만 표시되어 의도된 작업 환경 제공
2. **선택적 XREF 처리**: 필요할 때만 XREF 확장이 되어 불필요한 시각적 혼란 방지
3. **사용자 편의성**: 레이어 분리가 더 예측 가능하고 직관적으로 동작

## 호환성
- 기존의 모든 측정 모드(모델공간, 배치공간, 뷰포트)에서 정상 작동
- 기존 XREF 지원 기능은 그대로 유지하되 더 선택적으로 동작
- 레이어 상태 복원 및 에러 처리 로직 유지