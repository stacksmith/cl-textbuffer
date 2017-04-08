;; 
(in-package :cl-textbuffer)

;; 
;;
(define-condition gb-error (simple-error)
  ())

(define-condition gb-position-error (gb-error)
  ((gb :reader gb-position-error-gb
       :initarg :gb :initform nil)
   (position :reader gb-position-error-position
             :initarg :position :initform nil))
  (:report (lambda (condition stream)
             (format stream "Position ~D out of bounds in ~A"
                     (gb-position-error-position condition)
                     (gb-position-error-gb condition)))))

(defclass gapbuf (trivial-gray-streams:fundamental-character-output-stream)
  ((store :accessor store :accessor store :initform
	  (make-array 128 :adjustable t :element-type 'character :initial-element #\space))
   (gap-start :accessor gap-start :initform 0)
   (gap-end   :accessor gap-end   :initform 128)
   (buf-end   :accessor buf-end   :initform 128)
   (elwidth   :accessor elwidth   :initform 0)
   (resize-factor :accessor resize-factor :initform 2)))

(defun cursor-position (gb)
  (gap-start gb))

(defun print-object-gapbuf (gapbuf out)
  (with-slots (store gap-start gap-end) gapbuf
    ;(print-unreadable-object (gapbuf out :type t))
    (format out "gb:|~A|[~A]|~A|"
	    (subseq store 0 gap-start)
	    (- gap-end gap-start)
	    (subseq store gap-end (length store )))))
(defmethod print-object ((gapbuf gapbuf) out)
  (print-object-gapbuf gapbuf out)
  )

(declaim (inline gap-width gap-start gap-end))
(defun gap-width (gb)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (gap-start gap-end) gb
    (declare (fixnum gap-start gap-end))
    (the fixnum (- gap-end gap-start))))

(defun resize (gb)
  (with-slots (store resize-factor buf-end gap-end) gb
    (adjust-array store (* resize-factor buf-end))
    (let* ((new-buf-end (length store))
	   (increase (- new-buf-end buf-end))
	   (new-gap-end (+ gap-end increase)))
      (replace store store :start1 new-gap-end
	       :start2 gap-end :end2 buf-end)
      (setf buf-end new-buf-end
	    gap-end new-gap-end))))

(defun position-index (gb pos)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (inline gap-start gap-end))
  (with-slots (gap-start gap-end) gb
    (declare (fixnum gap-start gap-end pos))
    (if (< pos gap-start)
	pos
	(the fixnum (+ pos (gap-width gb))))))


(defmethod cursor-left ((gb gapbuf) cnt)
  (with-slots (store gap-start gap-end) gb
    (assert (<= cnt gap-start) ()
	    'gb-position-error :gb gb :position gap-start)
    (let ((src (- gap-start cnt))
	  (dst (- gap-end cnt)))
      (replace store store
	       :start1 dst :end1 gap-end
	       :start2 src)
      (setf gap-start src
	    gap-end   dst))))

(defmethod cursor-right ((gb gapbuf) cnt)
  (with-slots (store gap-start gap-end buf-end) gb
    (assert (<= cnt (- buf-end gap-end)) ()
	    'gb-position-error :gb gb :position gap-start)
    ;; cursor right means move from top of gap down
    (let ((end1 (+ gap-start cnt))
	  (end2 (+ gap-end cnt)))
      (replace store store
	       :start1 gap-start :end1 end1
	       :start2 gap-end )
      (setf gap-start end1
	    gap-end   end2))))
;;-----------------------------------------------------------------------------
;; Using cursor-left and cursor-right, cursor adjust picks one!
(defmethod cursor-adjust ((gb gapbuf) by)
  (unless (zerop by)
    (if (minusp by)
	(cursor-left gb (- by))
	(cursor-right gb by))))
(defmethod cursor-set ((gb gapbuf) pos)
  (with-slots (gap-start) gb
    (cursor-adjust gb (- pos gap-start))))


(defmethod gb-insert ((gb gapbuf) char)
  (with-slots (store gap-start gap-end elwidth) gb
    (assert (> (gap-width gb) 0) ()
	    'gb-position-error :gb gb :position gap-start)
    (setf (aref store gap-start) char)
    (incf gap-start)
    (incf elwidth)))

(defmethod gb-delete ((gb gapbuf))
  (with-slots (store gap-start gap-end buf-end elwidth) gb
    (assert (< gap-end buf-end) ()
	    'gb-position-error :gb gb :position gap-start)
    (incf gap-end)
    (decf elwidth)))

(defmethod trivial-gray-streams:stream-write-char ((gb gapbuf) char)
  (gb-insert gb char))

