(defun sbe ()
  (setvar "cmdecho" 0)

  (defun getArc (en)
    (command "lengthen" en "")
    (getvar "perimeter")
  )

  (defun getLine (en)
    (setq enlist (entget en))
    (distance (cdr (assoc 10 enlist)) (cdr (assoc 11 enlist)))
  )

  (defun getPoly (en)
    (command "area" "Object" en)
    (getvar "perimeter")
  )

  (if (setq eset (ssget))
      (progn
        (setq totalLen 0)
        (setq cntr 0)
        (while (< cntr (sslength eset))
          (setq en (ssname eset cntr))
          (setq enlist (entget en))
          (setq enType (cdr (assoc 0 enlist)))
          (cond
            ((= enType "ARC") (setq len (getArc en)))
            ((= enType "CIRCLE") (setq len (getPoly en)))
            ((= enType "ELLIPSE") (setq len (getPoly en)))
            ((= enType "LINE") (setq len (getLine en)))
            ((= enType "LWPOLYLINE") (setq len (getPoly en)))
            ((= enType "POLYLINE") (setq len (getPoly en)))
            ((= enType "SPLINE") (setq len (getPoly en)))
            (T (setq len 0.0))
          )
          (while (< (strlen enType) 12) (setq enType (strcat enType " ")))
          (princ "\n Found ")
          (princ enType)
          (princ " with a length of: ")
          (princ (rtos len))
          (setq totalLen (+ totalLen len))
          (setq cntr (+ cntr 1))
        )
      )
  )

  (setvar "cmdecho" 1)
  (alert (strcat "\n Found " (itoa cntr) " entitie(s) with a Total Length of " (rtos totalLen)))
  (princ)
)
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun split-string-to-lines (str)
  (vl-load-com)
  (setq lines '())
  (setq line "")
  (setq i 1)
  (setq leng (strlen str))
  (while (< (- i 1) leng)
    (setq ch (substr str i 1))
    (if (equal ch "\n")
      (progn
        (setq lines (cons line lines))
        (setq line ""))
      (setq line (strcat line ch)))
    (setq i (+ i 1)))
  (setq lines (cons line lines))
  (reverse lines))
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun ACAD2XL (XLTEXT /)
(vl-load-com) ;load extended AutoLISP functions (must be run prior to VL functions)
;get excel application
(setq XLapp (vlax-get-or-create-object "Excel.Application")
XLrun (vlax-get-property XLapp 'Visible)
)
;check if excel running
(if (= XLrun :vlax-false)
;excel is not running
(progn
(vlax-release-object XLapp)
(setq XLapp nil XLrun nil)
(gc)
(alert "\nERROR - Please Start Excel and select Cell (to place data)
prior to running this command.")
)
;excel is running
(progn
(setq XLwbk (vlax-get-property XLapp 'WorkBooks)
XLash (vlax-get-property XLapp 'ActiveSheet)
XLcel (vlax-get-property XLapp 'ActiveCell)
)
;check if workbook is active
(if (and XLash )
;excel workbook is active
(progn
(setq XLcol (vlax-get-property XLcel 'Column)
XLrow (vlax-get-property XLcel 'Row)
XLcva (vlax-get-property XLcel 'Text)
XLtxt (vlax-variant-value XLcva)
)
;convert column and row number into Cell text (Example - Column 1,Row 1 = A1

(if (= (/ XLcol 26) 0)
(setq TC (strcat (chr (+ (rem XLcol 26) 64)) (itoa XLrow)))
(setq TC (strcat (chr (+ (/ XLcol 26) 64)) (chr (+ (rem XLcol
26) 64)) (itoa XLrow)))
)
 

(setq CELL (strcat  TC":" TC  ))
   ;;(setq CELL1 (strcat (itoa 1)":" (itoa 1)))
;check if cell all ready has data
(if (= XLtxt "")
(setq YN "Y")
(setq YN (strcase (getstring (strcat "\nNOTICE - Excel Cell
contains [" XLtxt "] - Change [n]=No or [Y]=Yes ? "))))
)
(if (member YN (list "" "Y"))
 (progn
   
(setq Cells (vlax-get-property XLash 'Range CELL))
;;(setq del vlax-invoke-method Cells 'ClearContents)
;;(vlax-invoke-method Cells "Selection.ClearContents")
(setq VTXT (vlax-make-variant XLTEXT vlax-vbString))
   
(vlax-put-property Cells 'Value2 VTXT)
) 
  
  
)
;release objects
;;(vlax-release-object del)
;| (vlax-release-object Cells)

;;(vlax-release-object XLcel)
(vlax-release-object XLash)
(vlax-release-object XLwbk)
(vlax-release-object XLapp)
(setq XLapp nil XLrun nil XLwbk nil XLash nil XLcel nil Cells nil)
(gc)
   |;
)
;excel is running, but no active workbooks
(progn
(alert "\nERROR - Excel is running, but there are NO workbooks
open.")
(vlax-release-object XLwbk)
(vlax-release-object XLapp)
(setq XLapp nil XLrun nil XLwbk nil)
)
)
)
)
(princ)
)

;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun export-to-excel ()
  (vl-load-com)
  (setq xl (vlax-get-object "Excel.Application"))
  (setq workbooks (vlax-get-property xl "Workbooks"))
  (setq wb (vlax-invoke-method workbooks "Open" "E:\\golkhaneh\\adaption\\listo.xlsx"))
  (setq sheets (vlax-get-property wb "Sheets"))
  (setq sheet (vlax-get-property sheets "Item" 1))
  (vlax-put-property sheet "Name" "Data")
  (setq row 1 col 1)
  (setq sel (ssget "_X" '((0 . "TEXT")(8 . "steels"))))
  (setq contents "")
  (if sel
    (progn
      (setq nents (sslength sel))
      (setq i 0)
      (while (< i nents)
        (setq ent (ssname sel i))
        (setq contents (strcat contents (vla-get-TextString (vlax-ename->vla-object ent)) "\n"))
        (setq contents (substr contents 1 (1- (strlen contents)))) ; Remove the last newline character
        ;;(alert contents)
        (setq i (1+ i))))
    (prompt "\nNo text found in layer 'steels'."))
;; remove extra empty line

  (defun sort-by-substring (list substr)
  (vl-sort list (lambda (a b)
                  (string< (vl-string-search substr a) (vl-string-search substr b)))))

(setq sorted-lst (sort-by-substring contents "*"))
(princ sorted-lst)
  ;;
  (vl-load-com)
; ask for file path
(setq file_path (getfiled "Save CSV File" "" "csv" 1))

; create file
(setq file (open file_path "w"))
(write-line "name, Pos,Part Count, Size, @POS, Length,Total lengths(m), Total Weight(kg)" file)
; write contents to file
  
(setq lines (split-string-to-lines contents))

(foreach line lines
   
  (write-line line file)
)

; close file
(close file)

; release objects
(vlax-release-object sheet)
(vlax-release-object sheets)
(vlax-release-object wb)
(vlax-release-object workbooks)
(vlax-release-object xl)


 
  
;|    (foreach line lines
     (princ line)
  (ACAD2XL line)
  (alert (strcat "Exported " (itoa (strlen line)) " line to Excel successfully."))
) |;
 
    
  
        (ACAD2XL contents)  
    (princ a)
   

  
  )




;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun C:elu (/ tbl repeat-prompt)
  (setq j 1)
  (setq q0 (ssget "ALL" '((8 . "steels") (0 . "text"))))
  (setq no (sslength q0))
  (setq n 0)
  (setq total-length 0.0)
  (setq total-weight 0.0)
  (setq table-data '())
  (setq qq (ssname q0 n))
  (setq n (+ n 1))
  (setq q1 (entget qq))
  (setq q2 (assoc 1 q1))
  (setq q3 (cdr q2))
  (setq f (atof (substr q3 1 2)))
  (setq p (cdr (assoc 10 q1)))
  (setq hi (cdr (assoc 40 q1)))
  (setq n0 0)
  (setq qq0 (ssname q0 n0))
  (setq n0 (+ n0 1))
  (setq q10 (entget qq0))
  (setq q20 (assoc 1 q10))
  (setq q30 (cdr q20))
  (setq f0 (atof (substr q30 1 2)))
  (setq p0 (cdr (assoc 10 q10)))
  (setq opp "1")
                
  (while ( > j 0) 
    
      ;; Prompt the user to choose how to enter the length
(setq length-choice (getint "\nEnter the length of the line(s). or: \nfor select a line choose -1: [-1/N]"))
     
(alert (itoa length-choice) )
(if (< length-choice 0)        
    (progn
       (setq ss )
            (if (/= (sslength ss) 0)
                (progn
                  (setq nameOfObject (getstring "Enter name of object [BEAM/COLUMN/FOUNDATION/SLAB/WALL]"))
                  (sbe) ;; Call the ALE function
                  (setq position (getint "Enter the number of position : "  ))
                  (setq dist (getdist "Enter the length of space in which the reinforcements are placed in m: "))
                  (setq space (getreal "Enter the spacing between rebars in cm: "))
                  ; Calculate the number of rebars
                  (setq cmspace (/ space 100 ) )
                  (setq num (1+ (fix (/ dist cmspace))))
                  (setq tot (* num totalLen))
                  (setq diameters '("10" "12" "14" "16" "18" "20" "22" "25" "28" "30" "32"))
                  (setq dia (atoi(getstring t "\nSelect diameter [10/12/14/16/18/20/22/25/28/30/32]: ")))
                  (while (not (member  (rtos dia 2 0)  diameters))
                    (setq dia (getint "\nInvalid diameter. Enter again: "))
                  )
                  (setq total-length (+ total-length totalLen))
    (setq unit-weight (/ (cdr (assoc dia '((10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
    (setq total-weight (+ total-weight (* tot unit-weight)))
                  (setq finaltext (strcat nameOfObject "*" (itoa position) " " (rtos num 2 0) "\U+00D8" (rtos dia 2 0) "/" (rtos space 2 0) " L=" (rtos totalLen 2 2) " Total length is:" (rtos total-length 2 2) "m/ total weight is " (rtos total-weight 2 2) " kg\n" ))
                  (alert (strcat "this is string: " " "finaltext) )
                  
                  (vl-load-com)

                  (setq mspace (vla-get-modelspace 
                                    (vla-get-activedocument 
                                          (vlax-get-acad-object))))
                    ;;(setq currlayer (vla-get-layer doc))
                    (setq apt (getpoint "\nInsertion Point: "))

                    (setq LayerName "steels")
                    (defun get-text-height ()
  (while t
    (setq htstr (getstring "\nEnter text height: "))
    (cond
      ((and htstr (/= htstr "")) ; if the input is not empty
       (setq ht (atof htstr)) ; convert to a float
       (if (and ht (> ht 0)) ; check if it's a positive number
         (return ht) ; exit the loop and return the height
         (princ "\nInvalid height. Please enter a positive number.")))
      (t ; if the input is empty
       (princ "\nYou must enter a height.")))))

                    ;;(setq ht (getreal "\nHeight : "))
                  ;;(vla-put-layer finaltext  "steels")

                    (setq thetext (vla-AddText mspace finaltext 
                                                  (vlax-3d-point apt) (get-text-height)))
                  (vla-put-layer thetext LayerName )
                  
                  
                  )
              ;; no valid selection
              (progn
                (setq totalLen 0.0)
                (setq num 0)
                (alert "No valid object selected"))
            ))
  ;; User enters the length directly
  (progn
    (setq nameOfObject (getstring "Enter name of object [BEAM/COLUMN/FOUNDATION/SLAB/WALL]"))
    (setq position (getint "Enter the number of position : "  ))
    (setq totalLen length-choice)
    (setq dist (getdist "Enter the length of space in which the reinforcements are placed in m: "))
    (setq space (getreal "Enter the spacing between rebars in cm: "))
    ; Calculate the number of rebars
    (setq cmspace (/ space 100 ) )
    (setq num (1+ (fix (/ dist cmspace))))
    ;;(setq num (getint (strcat "Enter the number of rebars at position " opp ": ")))
    (setq tot (* num totalLen))
    (setq diameters '("10" "12" "14" "16" "18" "20" "22" "25" "28" "30" "32"))
  (setq dia (atoi(getstring t "\nSelect diameter [10/12/14/16/18/20/22/25/28/30/32]: ")))
  (while (not (member  (rtos dia 2 0)  diameters))
    (setq dia (getint "\nInvalid diameter. Enter again: "))
  )
     (setq total-length (+ total-length totalLen))
    (setq unit-weight (/ (cdr (assoc dia '((10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
    (setq total-weight (+ total-weight (* tot unit-weight)))
                  (setq finaltext (strcat  nameOfObject "*"  (itoa position) " " (rtos num 2 0) " \U+00D8" (rtos dia 2 0) "/" (rtos space 2 0) " L=" (rtos totalLen 2 2) " Total length is:" (rtos total-length 2 2) "m/ total weight is " (rtos total-weight 2 2) " kg\n" ))
    ;;(setq finaltext (strcat (itoa position) " " (rtos num 2 0) "\U+00D8" (rtos dia 2 0) "/" (rtos space 2 0) "L=" (rtos totalLen 2 2) ))
                  (alert (strcat "this is string: " " "finaltext) )
                  
                  (vl-load-com)

                  (setq mspace (vla-get-modelspace 
                                    (vla-get-activedocument 
                                          (vlax-get-acad-object))))
                    ;;(setq currlayer (vla-get-layer doc))
                    (setq apt (getpoint "\nInsertion Point: "))

                    (setq LayerName "steels")
                    
                    (setq ht (getreal "\nHeight : "))
                  ;;(vla-put-layer finaltext  "steels")

                    (setq thetext (vla-AddText mspace finaltext 
                                                  (vlax-3d-point apt) ht))
                  (vla-put-layer thetext LayerName )
                  ))
      
    (setq total-length (+ total-length totalLen))
    (setq unit-weight (/ (cdr (assoc dia '((10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
    (setq total-weight (+ total-weight (* tot unit-weight)))
      (setq table-data (append table-data (list (list totalLen num dia tot unit-weight (* tot unit-weight)))))

  ;; Prompt the user if they want to repeat
  (setq repeat-prompt (getstring t "\nDo you want to enter another length? [Y/N] exit [e]"))
  (if (or (equal repeat-prompt "Y") (equal repeat-prompt "y"))
      ;; User wants to repeat, so reset variables
      (progn
        (setq length-choice 0.0)
        (setq ss nil)
        (setq length1 0.0)
        (setq num 0)
        (setq tot 0.0)
        (setq dia 0.0))
    ;; User wants to exit, so display the table   
      (progn
      (setq msg (strcat "Total length: " (rtos total-length 2 2) " m\n"))
      (setq msg (strcat msg "Total weight: " (rtos total-weight 2 2) " kg\n"))
     
      )    
    )
    (if(or (equal repeat-prompt "E") (equal repeat-prompt "e"))
       (progn
      (setq msg (strcat "Total length: " (rtos total-length 2 2) " m\n"))
      (setq msg (strcat msg "Total weight: " (rtos total-weight 2 2) " kg\n"))
      (alert msg)
      (princ)
      (export-to-excel)
      (setq j 0))
    )
      )    
  )


