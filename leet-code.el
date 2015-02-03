;;; leet-code.el --- leet-code client for emacs

;;; Commentary:
;; 
;; login, submit, check result, retrieve quest info

;;; Code:

(eval-when-compile (require 'cl))

(require 'request)
(require 'json )

(defconst leet-code-base-url "https://oj.leetcode.com")
(defconst leet-code-action-login "accounts/login/")
(defconst leet-code-action-logout "accounts/logout/")
(defconst leet-code-request-header
  '(("Referer" . "https://oj.leetcode.com/")
    ("Host" . "oj.leetcode.com")
    ("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:35.0) Gecko/20100101 Firefox/35.0")))
(defconst leet-code-csrf-key "csrfmiddlewaretoken")
(defconst leet-code-csrf-cookie-key "csrftoken")
(defvar *leet-code-last-submission-id* nil)

;;; url-retrieve is good at dealing with cookie
(setq request-backend 'url-retrieve)


(defun leet-code--file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defconst leet-code-quests '(("reverse-words-in-a-string-ii/" 186)
                             ("largest-number/" 179)
                             ("dungeon-game/" 174)
                             ("binary-search-tree-iterator/" 173)
                             ("factorial-trailing-zeroes/" 172)
                             ("excel-sheet-column-number/" 171)
                             ("two-sum-iii-data-structure-design/" 170)
                             ("majority-element/" 169)
                             ("excel-sheet-column-title/" 168)
                             ("two-sum-ii-input-array-is-sorted/" 167)
                             ("fraction-to-recurring-decimal/" 166)
                             ("compare-version-numbers/" 165)
                             ("maximum-gap/" 164)
                             ("missing-ranges/" 163)
                             ("find-peak-element/" 162)
                             ("one-edit-distance/" 161)
                             ("intersection-of-two-linked-lists/" 160)
                             ("longest-substring-with-at-most-two-distinct-characters/" 159)
                             ("read-n-characters-given-read4-ii-call-multiple-times/" 158)
                             ("read-n-characters-given-read4/" 157)
                             ("binary-tree-upside-down/" 156)
                             ("min-stack/" 155)
                             ("find-minimum-in-rotated-sorted-array-ii/" 154)
                             ("find-minimum-in-rotated-sorted-array/" 153)
                             ("maximum-product-subarray/" 152)
                             ("reverse-words-in-a-string/" 151)
                             ("evaluate-reverse-polish-notation/" 150)
                             ("max-points-on-a-line/" 149)
                             ("sort-list/" 148)
                             ("insertion-sort-list/" 147)
                             ("lru-cache/" 146)
                             ("binary-tree-postorder-traversal/" 145)
                             ("binary-tree-preorder-traversal/" 144)
                             ("reorder-list/" 143)
                             ("linked-list-cycle-ii/" 142)
                             ("linked-list-cycle/" 141)
                             ("word-break-ii/" 140)
                             ("word-break/" 139)
                             ("copy-list-with-random-pointer/" 138)
                             ("single-number-ii/" 137)
                             ("single-number/" 136)
                             ("candy/" 135)
                             ("gas-station/" 134)
                             ("clone-graph/" 133)
                             ("palindrome-partitioning-ii/" 132)
                             ("palindrome-partitioning/" 131)
                             ("surrounded-regions/" 130)
                             ("sum-root-to-leaf-numbers/" 129)
                             ("longest-consecutive-sequence/" 128)
                             ("word-ladder/" 127)
                             ("word-ladder-ii/" 126)
                             ("valid-palindrome/" 125)
                             ("binary-tree-maximum-path-sum/" 124)
                             ("best-time-to-buy-and-sell-stock-iii/" 123)
                             ("best-time-to-buy-and-sell-stock-ii/" 122)
                             ("best-time-to-buy-and-sell-stock/" 121)
                             ("triangle/" 120)
                             ("pascals-triangle-ii/" 119)
                             ("pascals-triangle/" 118)
                             ("populating-next-right-pointers-in-each-node-ii/" 117)
                             ("populating-next-right-pointers-in-each-node/" 116)
                             ("distinct-subsequences/" 115)
                             ("flatten-binary-tree-to-linked-list/" 114)
                             ("path-sum-ii/" 113)
                             ("path-sum/" 112)
                             ("minimum-depth-of-binary-tree/" 111)
                             ("balanced-binary-tree/" 110)
                             ("convert-sorted-list-to-binary-search-tree/" 109)
                             ("convert-sorted-array-to-binary-search-tree/" 108)
                             ("binary-tree-level-order-traversal-ii/" 107)
                             ("construct-binary-tree-from-inorder-and-postorder-traversal/" 106)
                             ("construct-binary-tree-from-preorder-and-inorder-traversal/" 105)
                             ("maximum-depth-of-binary-tree/" 104)
                             ("binary-tree-zigzag-level-order-traversal/" 103)
                             ("binary-tree-level-order-traversal/" 102)
                             ("symmetric-tree/" 101)
                             ("same-tree/" 100)
                             ("recover-binary-search-tree/" 99)
                             ("validate-binary-search-tree/" 98)
                             ("interleaving-string/" 97)
                             ("unique-binary-search-trees/" 96)
                             ("unique-binary-search-trees-ii/" 95)
                             ("binary-tree-inorder-traversal/" 94)
                             ("restore-ip-addresses/" 93)
                             ("reverse-linked-list-ii/" 92)
                             ("decode-ways/" 91)
                             ("subsets-ii/" 90)
                             ("gray-code/" 89)
                             ("merge-sorted-array/" 88)
                             ("scramble-string/" 87)
                             ("partition-list/" 86)
                             ("maximal-rectangle/" 85)
                             ("largest-rectangle-in-histogram/" 84)
                             ("remove-duplicates-from-sorted-list/" 83)
                             ("remove-duplicates-from-sorted-list-ii/" 82)
                             ("search-in-rotated-sorted-array-ii/" 81)
                             ("remove-duplicates-from-sorted-array-ii/" 80)
                             ("word-search/" 79)
                             ("subsets/" 78)
                             ("combinations/" 77)
                             ("minimum-window-substring/" 76)
                             ("sort-colors/" 75)
                             ("search-a-2d-matrix/" 74)
                             ("set-matrix-zeroes/" 73)
                             ("edit-distance/" 72)
                             ("simplify-path/" 71)
                             ("climbing-stairs/" 70)
                             ("sqrtx/" 69)
                             ("text-justification/" 68)
                             ("add-binary/" 67)
                             ("plus-one/" 66)
                             ("valid-number/" 65)
                             ("minimum-path-sum/" 64)
                             ("unique-paths-ii/" 63)
                             ("unique-paths/" 62)
                             ("rotate-list/" 61)
                             ("permutation-sequence/" 60)
                             ("spiral-matrix-ii/" 59)
                             ("length-of-last-word/" 58)
                             ("insert-interval/" 57)
                             ("merge-intervals/" 56)
                             ("jump-game/" 55)
                             ("spiral-matrix/" 54)
                             ("maximum-subarray/" 53)
                             ("n-queens-ii/" 52)
                             ("n-queens/" 51)
                             ("powx-n/" 50)
                             ("anagrams/" 49)
                             ("rotate-image/" 48)
                             ("permutations-ii/" 47)
                             ("permutations/" 46)
                             ("jump-game-ii/" 45)
                             ("wildcard-matching/" 44)
                             ("multiply-strings/" 43)
                             ("trapping-rain-water/" 42)
                             ("first-missing-positive/" 41)
                             ("combination-sum-ii/" 40)
                             ("combination-sum/" 39)
                             ("count-and-say/" 38)
                             ("sudoku-solver/" 37)
                             ("valid-sudoku/" 36)
                             ("search-insert-position/" 35)
                             ("search-for-a-range/" 34)
                             ("search-in-rotated-sorted-array/" 33)
                             ("longest-valid-parentheses/" 32)
                             ("next-permutation/" 31)
                             ("substring-with-concatenation-of-all-words/" 30)
                             ("divide-two-integers/" 29)
                             ("implement-strstr/" 28)
                             ("remove-element/" 27)
                             ("remove-duplicates-from-sorted-array/" 26)
                             ("reverse-nodes-in-k-group/" 25)
                             ("swap-nodes-in-pairs/" 24)
                             ("merge-k-sorted-lists/" 23)
                             ("generate-parentheses/" 22)
                             ("merge-two-sorted-lists/" 21)
                             ("valid-parentheses/" 20)
                             ("remove-nth-node-from-end-of-list/" 19)
                             ("4sum/" 18)
                             ("letter-combinations-of-a-phone-number/" 17)
                             ("3sum-closest/" 16)
                             ("3sum/" 15)
                             ("longest-common-prefix/" 14)
                             ("roman-to-integer/" 13)
                             ("integer-to-roman/" 12)
                             ("container-with-most-water/" 11)
                             ("regular-expression-matching/" 10)
                             ("palindrome-number/" 9)
                             ("string-to-integer-atoi/" 8)
                             ("reverse-integer/" 7)
                             ("zigzag-conversion/" 6)
                             ("longest-palindromic-substring/" 5)
                             ("median-of-two-sorted-arrays/" 4)
                             ("longest-substring-without-repeating-characters/" 3)
                             ("add-two-numbers/" 2)
                             ("two-sum/" 1)))

(defconst leet-code-status-map
  '((10 "AC" )
   (11 "WA" )
   (12 "MLE" )
   (13 "OLE" )
   (14 "TLE" )
   (15 "RE" )
   (16 "IE" )
   (20 "CE" )
   (21 "UE" )))


(defun leet-code--fresh-request ()
  (if (eq request-backend 'url-retrieve)
      (url-cookie-clean-up)
    (when (file-exists-p (request--curl-cookie-jar))
      (delete-file (request--curl-cookie-jar))))

  (request leet-code-base-url
           :sync t
           :headers leet-code-request-header
           ))

(defun leet-code--login (uname pwd)
  (interactive
   (list
    (read-string "input username or email ")
    (read-passwd "intpu password ")
    )
   )
  (unless (leet-code--is-loggedin)
    (leet-code--fresh-request)
    (request (concat leet-code-base-url "/" leet-code-action-login)
             :type "POST"
             :data `(("login" . ,uname )
                     ("password" . ,pwd)
                     ("Host" . "oj.leetcode.com")
                     (,leet-code-csrf-key .
                                          ,(cdr
                                            (assoc
                                             leet-code-csrf-cookie-key
                                             (request-cookie-alist
                                              "oj.leetcode.com" "/" t)))))
             :headers leet-code-request-header)))

(defun leet-code--is-loggedin ()
  (string-match "Successfully signed in"
                (or (cdr
                     (assoc
                      "messages"
                      (request-cookie-alist "oj.leetcode.com" "/" t))) "")))


(defun leet-code--logout ()
  (interactive)
  (request (concat leet-code-base-url "/" leet-code-action-logout)
           :sync t
           :headers leet-code-request-header))


(defun* leet-code--submit
    (content &optional qname qid &key (type "large") (lang "cpp"))  
  (assert (leet-code--is-loggedin) t "login first")
  (unless (and qname qid)
    (let ((quest
           (assoc
            (completing-read "choose quest: " leet-code-quests nil t) 
            leet-code-quests
            )))
      (if quest
          (setq qname (car quest)  qid (cadr quest))
          (error "invalid quest"))
      )
    )
   
   (request
    (concat
     leet-code-base-url
     "/problems/" qname "/submit/" )
    :type "POST"
    :data `((,leet-code-csrf-key .
                                 ,(cdr (assoc leet-code-csrf-cookie-key
                                              (request-cookie-alist "oj.leetcode.com" "/" t)) )
                                 )
            ("lang" . ,lang)
            ("data_input" . "")
            ("question_id" . ,(number-to-string qid))
            ("judge_type" . ,type);large, small ?
            ("typed_code" . ,content)
            )
    :headers leet-code-request-header
    :parser 'json-read
    :success
    (function* (lambda (&key data &allow-other-keys)
                 (setq *leet-code-last-submission-id*
                       (number-to-string
                        (cdr (assoc 'submission_id data))))
                 (leet-code--check-submission)))))


(defun leet-code--submit-region (start end)
  (interactive "r")
  (leet-code--submit (buffer-substring start end)))


(defun leet-code--submit-file (file)
  (interactive (list
                (read-file-name
                 "select file: "
                 default-directory
                 (file-name-nondirectory buffer-file-name))))
  (leet-code--submit (leet-code--file-to-string file)))



(defun leet-code--check-submission (&optional sid)
  (assert (leet-code--is-loggedin) t "login first")
  (unless sid
    (setq sid *leet-code-last-submission-id*))
  (request
   (concat leet-code-base-url "/submissions/detail/" sid "/check/")
   :parser 'json-read
   :headers leet-code-request-header
   :success
   (function* (lambda (&key data &allow-other-keys)
                (let ((status-code (cdr (assoc 'status_code data)))
                      (state (cdr (assoc 'state data)))
                      )
                  (if (not (string= state "SUCCESS"))
                      (run-with-idle-timer
                       3
                       nil
                       'leet-code--check-submission)
                      (message (cadr
                            (assoc
                             status-code
                             leet-code-status-map)))))))))

(defun leet-code--parse-code (raw-content)
  (let ((re
         ;; I don't want to do this... but they don't provide me some API
         "<div\s*?class=\"col-md-12\"\s*?ng-init=\"init(\\(.*\\),\s*?'\\w+');\">"
         )
        code-str
        code-obj)
    (string-match re raw-content)
    (setq code-str (match-string-no-properties 1 raw-content))
    (with-temp-buffer
      (insert code-str)
      (while (search-backward "'" (point-min) t)
        (replace-match "\""))
      (goto-char 0)
      (setq code-obj (json-read))
      )
    (when (not (stringp code-obj))      ;no error thrown and result isn't str.
      (let ((cpp-entry                  ;why hard code? java devs use emacs are
                                        ;rarely, if you want other lang, send me
                                        ;PR or raise a issue. 
             (car (delq nil
                        (mapcar (lambda (entry)
                                  (and
                                   (string= (cdr (assoc 'value entry)) "cpp" )
                                   entry)) code-obj)))))
        (cdr
         (assoc 'defaultCode cpp-entry))))))

(defun leet-code--parse-info (raw-content)
  (substring-no-properties
   raw-content
   (string-match "<div\s?class=\"question-content\">" raw-content) ;malformatted
                                                                   ;html? it's
                                                                   ;ok to libxml
   (string-match "<div\s?id=\"tags\"\s?" raw-content)))

(defun leet-code--display-info (qname)
  (interactive
   (list
    (completing-read
     "choose question: "
     leet-code-quests
     nil
     t)))
  (let* ((data
          (request-response-data
           (request
            (concat leet-code-base-url "/problems/" qname "/")
            :parser 'buffer-string
            :headers leet-code-request-header
            :sync t
            )
           ))
         (info (leet-code--parse-info data))
         (start (point-min))
         (end (length info))
        )
    (save-excursion
      (goto-char start)
      (insert info)
      (newline)
      (shr-render-region start end)
      (comment-region start (point))
;      (fill-region start (point))
      (when (y-or-n-p "insert code template? ")
        (insert (leet-code--parse-code data))
        (newline)
          ))
    ))


(provide 'leet-code)

;;; leet-code.el ends here
