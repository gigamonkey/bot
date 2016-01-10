(in-package :com.gigamonkeys.bot)

(defclass circular-buffer ()
  ((items :accessor items)
   (insert-index :initform 0 :accessor insert-index)
   (item-count :initform 0 :accessor item-count)))

(defmethod initialize-instance :after ((object circular-buffer) &key size &allow-other-keys)
  (setf (items object) (make-array size)))

(defmethod add-item (item (buffer circular-buffer))
  (with-slots (items insert-index item-count) buffer
    (setf (aref items insert-index) item)
    (setf insert-index (mod (1+ insert-index) (length items)))
    (setf item-count (min (1+ item-count) (length items)))
    item))

(defun to-list (buffer)
  (let ((result ()))
    (map-items-backward (lambda (item) (push item result)) buffer)
    result))

(defmethod map-items-forward (fn buffer)
  (with-slots (items insert-index item-count) buffer
    (loop repeat item-count
       for i = (start-index buffer) then (mod (1+ i) item-count)
       do (funcall fn (aref items i)))))

(defmethod map-items-backward (fn buffer)
  (with-slots (items insert-index item-count) buffer
    (loop repeat item-count
       for i = (end-index buffer) then (mod (1- i) item-count)
       do (funcall fn (aref items i)))))

(defmethod start-index (buffer)
  (with-slots (items insert-index item-count) buffer
    (if (= item-count (length items))
	(mod insert-index (length items))
	0)))

(defmethod end-index (buffer)
  (with-slots (items insert-index item-count) buffer
    (if (= item-count (length items))
	(mod (1- insert-index) (length items))
	(1- item-count))))

      