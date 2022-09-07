(defun preamble (level gender side)
  "Return the preamble for the opnote of lumbar decompression."
  (let ((subj (case gender (m "he") (f "she")))
        (genitive (case gender (m "his") (f "her")))
        (upper (multiple-value-bind (first second)
                   (cl-ppcre:scan-to-strings "(.*)/.*" level)
                 (elt second 0))))
    (format nil "After induction of general anesthesia, ~a was placed in prone position with ~a hip in slight flexion.  The level of ~a was confirmed on a lateral X-ray, and was marked with indigo carmine.  An operating microscope was used from the beginning of the procedure.  After routine prepping and draping, a linear skin incision with a length of 7-cm was made slightly off the midline to the ~a side.  The dorsolumbar fascia was incised on the ~a side.  The multifidus muscles were dissected off the spinous processes to expose the ~a ~a interlaminar space.  With a high-speed drill and SONOPET, partial hemilaminectomy with limited medial facetectomy was done.  The ~a yellow ligament was exposed in its entirety, and was removed piece by piece leaving the most lateral portion for later removal.  Then, slanting the microscope angle, we approached the contralateral side.  Removing the bone at the base of the spinous processes and the under-surface of the contralateral ~a lamina.
The wound was irrigated with copious amount of saline.  The fascia was closed with 2-0 Surgilon.  The skin was closed with subcutaneous sutures of 3-0 Vicryl and surgical tapes.  The patient tolerated this procedure well, and was transferred to the ward in stable condition."
            subj genitive level side side side level side upper)))

(defun decomp-op (level gender side)
  "Return the preamble for the opnote of lumbar decompression."
  (let ((subj (case gender (m "he") (f "she")))
        (genitive (case gender (m "his") (f "her")))
        (upper (upper level)))
    (format "After induction of general anesthesia, ~a was placed in prone position with ~a hip in slight flexion.  The level of ~a was confirmed on a lateral X-ray, and was marked with indigo carmine.  An operating microscope was used from the beginning of the procedure.  After routine prepping and draping, a linear skin incision with a length of 7-cm was made slightly off the midline to the ~a side.  The dorsolumbar fascia was incised on the ~a side.  The multifidus muscles were dissected off the spinous processes to expose the ~a ~a interlaminar space.  With a high-speed drill and SONOPET, partial hemilaminectomy with limited medial facetectomy was done.  The ~a yellow ligament was exposed in its entirety, and was removed piece by piece leaving the most lateral portion for later removal.  Then, slanting the microscope angle, we approached the contralateral side.  Removing the bone at the base of the spinous processes and the under-surface of the contralateral ~a lamina.
The wound was irrigated with copious amount of saline.  The fascia was closed with 2-0 Surgilon.  The skin was closed with subcutaneous sutures of 3-0 Vicryl and surgical tapes.  The patient tolerated this procedure well, and was transferred to the ward in stable condition."
            subj genitive level side side side level side upper)))