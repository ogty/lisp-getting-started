(defclass fundamental-test ()
  ((test-form :initarg :form
              :accessor test-form)
   (test-value :initarg :value
               :accessor test-value)
   (test-result :initform nil
                :accessor test-result)
   (test-equal :initform #'equal
               :initarg :test
               :accessor test-equal)))
(defgeneric run-test (obj))
(defmethod run-test ((obj fundamental-test))
  (if (funcall (test-equal obj)
               (apply (car (test-form obj))
                      (cdr (test-form obj)))
               (test-value obj))
      t
      nil))

(defclass hash-mixin ()
  ((key :initarg :key :accessor key)
   (table :accessor table
          :allocation :class
          :initform (make-hash-table :test #'equal))))

(defclass test-hash-mixin (hash-mixin)
  ((table :accessor table :allocation :class
               :initform (make-hash-table :test #'equal))))

(defclass unit-test (test-hash-mixin fundamental-test) ())

(defmethod initialize-instance :after ((obj hash-mixin) &key)
  (setf (gethash (key obj) (table obj)) obj))

(defun read-test (obj)
  (gethash (key obj) (table obj)))

(defmethod (setf key) :before (key (obj hash-mixin))
  (setf (gethash (key obj) (table obj)) nil))
(defmethod (setf key) :after (key (obj hash-mixin))
  (setf (gethash (key obj) (table obj)) obj))

(defun delete-test (obj)
  (setf (gethash (key obj) (table obj)) nil))

