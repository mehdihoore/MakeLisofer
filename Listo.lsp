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


(vl-load-com)

(defun c:openExcel (/ path filename)
  (if (findfile (setq path (vl-string-right-trim "\\" (getvar 'dwgprefix))
                      path (substr path 1 (vl-string-position (ascii "\\") path 0 T))
                      filename (strcat "\"" path "\\QCL Issue comment.xlsm\"")))
    (startapp "C:\\Program Files\\Microsoft Office\\root\Office16\\EXCEL.EXE" filename )
    (princ "\n'QCL Issue Register' File not found."))
(princ))
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun export-to-excel ()
  (vl-load-com)
  (setq xl (vlax-get-object "Excel.Application"))
  
  (if xl 
    (progn
    (vlax-put-property xl 'visible :vlax-true)
  (setq workbooks (vlax-get-property xl "workbooks"))
  (setq address (getstring "Give the address of excel file: "))
  (setq wb (vlax-invoke-method workbooks "Open" address))
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
(setq lines (split-string-to-lines contents))
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
  (progn
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


(setq lines (split-string-to-lines contents))
    (defun sort-by-number-after-star (list)
  (vl-sort-i list
             (lambda (a b)
               (let* ((num-a (nth 1 (vl-string-split a "*")))
                      (num-b (nth 1 (vl-string-split b "*"))))
                 (< (atoi num-a) (atoi num-b))))))
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

  
  )
  
  ))
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun put-text ()
  
    (setq nameOfObject (getstring "Enter name of object [BEAM/COLUMN/FOUNDATION/SLAB/WALL]"))
    (setq position (getint "Enter the number of position : "  ))
  (setq length-choice (getreal "Enter length of rebar or Enter [0] to select"))
  (if (= length-choice 0)
    (progn
    (sbe)
    (setq length-choice totalLen )
    (alert  (rtos length-choice)  ))
  )
    (setq totalLen length-choice)
(princ totalLen)
    (setq dist (getdist "Enter the length of space in which the reinforcements are placed in m: "))
(princ totalLen)
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
     ;;(setq total-length (+ total-length totalLen))
    (setq unit-weight (/ (cdr (assoc dia '((10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
   (setq total-weight  (* tot unit-weight))
                  (setq finaltext (strcat  nameOfObject "*"  (itoa position) " " (rtos num 2 0) " \U+00D8" (rtos dia 2 0) "/" (rtos space 2 0) " L=" (rtos totalLen 2 2) " Total length is:" (rtos tot 2 2) "m/ total weight is " (rtos total-weight 2 2) " kg\n" ))
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
                 
)

;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun create-layer (layer-name)
  (setq layer-table (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  ;;(setq layer-name (getstring "Give the layer name: "))
  (setq layer (tblsearch "layer" layer-name))
  (if (not layer)
    (progn
      (setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
      (setq layer (vla-add layers layer-name))
      ;;(vla-put-Color layer acMagenta)
      (vla-put-Color layer acMagenta))) layer)
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun C:elu (/ tbl repeat-prompt)
  (create-layer "steels")
  (setq j 1)
  (setq q0 (ssget "ALL" '((8 . "steels") (0 . "text"))))

  (setq opp "1")
                
  (while ( > j 0) 
(put-text)
  ;; Prompt the user if they want to repeat
  (setq repeat-prompt (getstring t "\nDo you want to enter another length? [Y] or exit [e]"))
   
     
  
    (if(or (equal repeat-prompt "E") (equal repeat-prompt "e"))
       (progn
     
      (export-to-excel)
      (setq j 0))
    )
      )    
  )


