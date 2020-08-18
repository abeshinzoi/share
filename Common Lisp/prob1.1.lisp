;; prob1.1.lisp
;; �ړI�F�O�p�����������ł���悤�Ȏ��R����������D
;; �쐬�ҁF@abeshinzoi
;; �쐬���F2020-08-18




;; squarep
;; �^����ꂽ���R�� n �������������ׂ�D
;; ���� n ���������Ȃ� ��n ��Ԃ��C�������łȂ��Ȃ� nil ��Ԃ��D
;; �o�O�F��n �����R���ɔ��ɋ߂��Ƃ��ԈႦ��D
(defun squarep (n)
  (multiple-value-bind (a b) (floor (sqrt n))
                       (if (= b 0) a nil) ))
;; sum
;; 1+2+...+n ���v�Z����D
(defun sum (n)
  (/ (* n (+ n 1)) 2) )

;; search1
;; 1+2+...+lower ���� 1+2+...+upper �܂łɕ����������邩���ׂ�D
;; ���������������炻�̎��_�ŃX�g�b�v���āC���Ƃ���
;; 1+2+...+n ���ŏ��̕�������������C
;; n �� ��(1+2+...+n) ��g�ɂ��ĕԂ��D
;; ���������Ȃ���� nil ��Ԃ��D
(defun search1 (lower upper)
  (if (<= lower upper)
      (let ((sp (squarep (sum lower))))
        (if sp
            (list lower sp)
          (search1 (1+ lower) upper) ))
        nil) )
;; search2
;; k^2=lower^2~upper^2 �ɑ΂��� (k^2-1)/2 �� (k^2+1)/2 �������������ׂ�D
(defun search2 (lower upper)
  (if (<= lower upper)
      (let* ((k^2 (* lower lower))
             (sp- (squarep (/ (- k^2 1) 2)))
             (sp+ (squarep (/ (+ k^2 1) 2))) )
        (if (or sp- sp+)
            (list lower sp- sp+)
          (search2 (1+ lower) upper) ))
    nil) )
