;; prob1.1.lisp
;; 目的：三角数かつ平方数であるような自然数を見つける．
;; 作成者：@abeshinzoi
;; 作成日：2020-08-18




;; squarep
;; 与えられた自然数 n が平方数か調べる．
;; もし n が平方数なら √n を返し，平方数でないなら nil を返す．
;; バグ：√n が自然数に非常に近いとき間違える．
(defun squarep (n)
  (multiple-value-bind (a b) (floor (sqrt n))
                       (if (= b 0) a nil) ))
;; sum
;; 1+2+...+n を計算する．
(defun sum (n)
  (/ (* n (+ n 1)) 2) )

;; search1
;; 1+2+...+lower から 1+2+...+upper までに平方数があるか調べる．
;; 平方数があったらその時点でストップして，たとえば
;; 1+2+...+n が最初の平方数だったら，
;; n と √(1+2+...+n) を組にして返す．
;; 平方数がなければ nil を返す．
(defun search1 (lower upper)
  (if (<= lower upper)
      (let ((sp (squarep (sum lower))))
        (if sp
            (list lower sp)
          (search1 (1+ lower) upper) ))
        nil) )
;; search2
;; k^2=lower^2~upper^2 に対して (k^2-1)/2 か (k^2+1)/2 が平方数か調べる．
(defun search2 (lower upper)
  (if (<= lower upper)
      (let* ((k^2 (* lower lower))
             (sp- (squarep (/ (- k^2 1) 2)))
             (sp+ (squarep (/ (+ k^2 1) 2))) )
        (if (or sp- sp+)
            (list lower sp- sp+)
          (search2 (1+ lower) upper) ))
    nil) )
