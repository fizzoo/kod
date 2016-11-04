(defgeneric real-part (x))
(defgeneric imag-part (x))
(defgeneric magnitude (x))
(defgeneric angle (x))

(defclass rectangular-comp ()
  ((real
    :initarg :real)
   (imaginary
    :initarg :imaginary)))
(defclass polar-comp ()
  ((magnitude
    :initarg :magnitude)
   (angle
    :initarg :angle)))

(make-instance 'rectangular-comp :real 1 :imaginary 1)

(defun make-polar-comp (magnitude angle)
  (make-instance 'polar-comp :magnitude magnitude :angle angle))
(defun make-rectangular-comp (real imaginary)
  (make-instance 'rectangular-comp :real real :imaginary imaginary))

(make-polar-comp 1 1)

(defmethod real-part ((comp rectangular-comp))
  (slot-value comp 'real))
(defmethod real-part ((comp polar-comp))
  (with-slots (magnitude angle) comp
    (* magnitude (cos angle))))

(real-part (make-rectangular-comp 1 2))
(real-part (make-polar-comp 1 0))
(real-part (make-polar-comp 1 pi))

