;;; leet-code.el --- leet-code client for emacs

;;; Commentary:
;; 
;; login, submit, check result, retrieve quest info

;;; Code:

(eval-when-compile (require 'cl))
(require-package 'request)
(require 'request)
(require 'json )

(defconst leet-code-base-domain "leetcode.com")
(defconst leet-code-base-url (concat "https://" leet-code-base-domain))
(defconst leet-code-action-login "accounts/login/")
(defconst leet-code-action-logout "accounts/logout/")
(defconst leet-code-request-header
  `(("Referer" . ,leet-code-base-url)
    ("Host" . ,leet-code-base-domain)
    ("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:35.0) Gecko/20100101 Firefox/35.0")))
(defconst leet-code-csrf-key "csrfmiddlewaretoken")
(defconst leet-code-csrf-cookie-key "csrftoken")
(defconst leet-code-quest-cleaner "\\(\\[\[^\\]+?\]\s\\)")
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
(defvar leet-code-default-lang "cpp" "Your prefered programming language.")

(defvar *leet-code-last-submission-id* nil)

;;; url-retrieve is good at dealing with cookie
(setq request-backend 'url-retrieve)


(defun leet-code--file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun leet-code--get-cookie-val (key)
  "Get cookie value by KEY."
  (cdr
   (assoc
    key
    (request-cookie-alist
     leet-code-base-domain "/" t))))

;;; evulate this in firefox's developer tool to get latest problem lists.
;;;$x("//table[@id='problemList']/tbody/tr/td[2]/text()|//td[3]/a/@href|//td[5]/text()").reverse().map(function(e, i){return ["(\"[{0}]", "{1}\"", "{2})\n"][i % 3].replace("{"+ i % 3 +"}", e.nodeValue);}).join(" ")
(defconst leet-code-quests '(
                             ("[Medium] /problems/two-sum/" 1)
                             ("[Medium] /problems/add-two-numbers/" 2)
                             ("[Medium] /problems/longest-substring-without-repeating-characters/" 3)
                             ("[Hard] /problems/median-of-two-sorted-arrays/" 4)
                             ("[Medium] /problems/longest-palindromic-substring/" 5)
                             ("[Easy] /problems/zigzag-conversion/" 6)
                             ("[Easy] /problems/reverse-integer/" 7)
                             ("[Easy] /problems/string-to-integer-atoi/" 8)
                             ("[Easy] /problems/palindrome-number/" 9)
                             ("[Hard] /problems/regular-expression-matching/" 10)
                             ("[Medium] /problems/container-with-most-water/" 11)
                             ("[Medium] /problems/integer-to-roman/" 12)
                             ("[Easy] /problems/roman-to-integer/" 13)
                             ("[Easy] /problems/longest-common-prefix/" 14)
                             ("[Medium] /problems/3sum/" 15)
                             ("[Medium] /problems/3sum-closest/" 16)
                             ("[Medium] /problems/letter-combinations-of-a-phone-number/" 17)
                             ("[Medium] /problems/4sum/" 18)
                             ("[Easy] /problems/remove-nth-node-from-end-of-list/" 19)
                             ("[Easy] /problems/valid-parentheses/" 20)
                             ("[Easy] /problems/merge-two-sorted-lists/" 21)
                             ("[Medium] /problems/generate-parentheses/" 22)
                             ("[Hard] /problems/merge-k-sorted-lists/" 23)
                             ("[Medium] /problems/swap-nodes-in-pairs/" 24)
                             ("[Hard] /problems/reverse-nodes-in-k-group/" 25)
                             ("[Easy] /problems/remove-duplicates-from-sorted-array/" 26)
                             ("[Easy] /problems/remove-element/" 27)
                             ("[Easy] /problems/implement-strstr/" 28)
                             ("[Medium] /problems/divide-two-integers/" 29)
                             ("[Hard] /problems/substring-with-concatenation-of-all-words/" 30)
                             ("[Medium] /problems/next-permutation/" 31)
                             ("[Hard] /problems/longest-valid-parentheses/" 32)
                             ("[Hard] /problems/search-in-rotated-sorted-array/" 33)
                             ("[Medium] /problems/search-for-a-range/" 34)
                             ("[Medium] /problems/search-insert-position/" 35)
                             ("[Easy] /problems/valid-sudoku/" 36)
                             ("[Hard] /problems/sudoku-solver/" 37)
                             ("[Easy] /problems/count-and-say/" 38)
                             ("[Medium] /problems/combination-sum/" 39)
                             ("[Medium] /problems/combination-sum-ii/" 40)
                             ("[Hard] /problems/first-missing-positive/" 41)
                             ("[Hard] /problems/trapping-rain-water/" 42)
                             ("[Medium] /problems/multiply-strings/" 43)
                             ("[Hard] /problems/wildcard-matching/" 44)
                             ("[Hard] /problems/jump-game-ii/" 45)
                             ("[Medium] /problems/permutations/" 46)
                             ("[Hard] /problems/permutations-ii/" 47)
                             ("[Medium] /problems/rotate-image/" 48)
                             ("[Medium] /problems/anagrams/" 49)
                             ("[Medium] /problems/powx-n/" 50)
                             ("[Hard] /problems/n-queens/" 51)
                             ("[Hard] /problems/n-queens-ii/" 52)
                             ("[Medium] /problems/maximum-subarray/" 53)
                             ("[Medium] /problems/spiral-matrix/" 54)
                             ("[Medium] /problems/jump-game/" 55)
                             ("[Hard] /problems/merge-intervals/" 56)
                             ("[Hard] /problems/insert-interval/" 57)
                             ("[Easy] /problems/length-of-last-word/" 58)
                             ("[Medium] /problems/spiral-matrix-ii/" 59)
                             ("[Medium] /problems/permutation-sequence/" 60)
                             ("[Medium] /problems/rotate-list/" 61)
                             ("[Medium] /problems/unique-paths/" 62)
                             ("[Medium] /problems/unique-paths-ii/" 63)
                             ("[Medium] /problems/minimum-path-sum/" 64)
                             ("[Hard] /problems/valid-number/" 65)
                             ("[Easy] /problems/plus-one/" 66)
                             ("[Easy] /problems/add-binary/" 67)
                             ("[Hard] /problems/text-justification/" 68)
                             ("[Medium] /problems/sqrtx/" 69)
                             ("[Easy] /problems/climbing-stairs/" 70)
                             ("[Medium] /problems/simplify-path/" 71)
                             ("[Hard] /problems/edit-distance/" 72)
                             ("[Medium] /problems/set-matrix-zeroes/" 73)
                             ("[Medium] /problems/search-a-2d-matrix/" 74)
                             ("[Medium] /problems/sort-colors/" 75)
                             ("[Hard] /problems/minimum-window-substring/" 76)
                             ("[Medium] /problems/combinations/" 77)
                             ("[Medium] /problems/subsets/" 78)
                             ("[Medium] /problems/word-search/" 79)
                             ("[Medium] /problems/remove-duplicates-from-sorted-array-ii/" 80)
                             ("[Medium] /problems/search-in-rotated-sorted-array-ii/" 81)
                             ("[Medium] /problems/remove-duplicates-from-sorted-list-ii/" 82)
                             ("[Easy] /problems/remove-duplicates-from-sorted-list/" 83)
                             ("[Hard] /problems/largest-rectangle-in-histogram/" 84)
                             ("[Hard] /problems/maximal-rectangle/" 85)
                             ("[Medium] /problems/partition-list/" 86)
                             ("[Hard] /problems/scramble-string/" 87)
                             ("[Easy] /problems/merge-sorted-array/" 88)
                             ("[Medium] /problems/gray-code/" 89)
                             ("[Medium] /problems/subsets-ii/" 90)
                             ("[Medium] /problems/decode-ways/" 91)
                             ("[Medium] /problems/reverse-linked-list-ii/" 92)
                             ("[Medium] /problems/restore-ip-addresses/" 93)
                             ("[Medium] /problems/binary-tree-inorder-traversal/" 94)
                             ("[Medium] /problems/unique-binary-search-trees-ii/" 95)
                             ("[Medium] /problems/unique-binary-search-trees/" 96)
                             ("[Hard] /problems/interleaving-string/" 97)
                             ("[Medium] /problems/validate-binary-search-tree/" 98)
                             ("[Hard] /problems/recover-binary-search-tree/" 99)
                             ("[Easy] /problems/same-tree/" 100)
                             ("[Easy] /problems/symmetric-tree/" 101)
                             ("[Easy] /problems/binary-tree-level-order-traversal/" 102)
                             ("[Medium] /problems/binary-tree-zigzag-level-order-traversal/" 103)
                             ("[Easy] /problems/maximum-depth-of-binary-tree/" 104)
                             ("[Medium] /problems/construct-binary-tree-from-preorder-and-inorder-traversal/" 105)
                             ("[Medium] /problems/construct-binary-tree-from-inorder-and-postorder-traversal/" 106)
                             ("[Easy] /problems/binary-tree-level-order-traversal-ii/" 107)
                             ("[Medium] /problems/convert-sorted-array-to-binary-search-tree/" 108)
                             ("[Medium] /problems/convert-sorted-list-to-binary-search-tree/" 109)
                             ("[Easy] /problems/balanced-binary-tree/" 110)
                             ("[Easy] /problems/minimum-depth-of-binary-tree/" 111)
                             ("[Easy] /problems/path-sum/" 112)
                             ("[Medium] /problems/path-sum-ii/" 113)
                             ("[Medium] /problems/flatten-binary-tree-to-linked-list/" 114)
                             ("[Hard] /problems/distinct-subsequences/" 115)
                             ("[Medium] /problems/populating-next-right-pointers-in-each-node/" 116)
                             ("[Hard] /problems/populating-next-right-pointers-in-each-node-ii/" 117)
                             ("[Easy] /problems/pascals-triangle/" 118)
                             ("[Easy] /problems/pascals-triangle-ii/" 119)
                             ("[Medium] /problems/triangle/" 120)
                             ("[Medium] /problems/best-time-to-buy-and-sell-stock/" 121)
                             ("[Medium] /problems/best-time-to-buy-and-sell-stock-ii/" 122)
                             ("[Hard] /problems/best-time-to-buy-and-sell-stock-iii/" 123)
                             ("[Hard] /problems/binary-tree-maximum-path-sum/" 124)
                             ("[Easy] /problems/valid-palindrome/" 125)
                             ("[Hard] /problems/word-ladder-ii/" 126)
                             ("[Medium] /problems/word-ladder/" 127)
                             ("[Hard] /problems/longest-consecutive-sequence/" 128)
                             ("[Medium] /problems/sum-root-to-leaf-numbers/" 129)
                             ("[Medium] /problems/surrounded-regions/" 130)
                             ("[Medium] /problems/palindrome-partitioning/" 131)
                             ("[Hard] /problems/palindrome-partitioning-ii/" 132)
                             ("[Medium] /problems/clone-graph/" 133)
                             ("[Medium] /problems/gas-station/" 134)
                             ("[Hard] /problems/candy/" 135)
                             ("[Medium] /problems/single-number/" 136)
                             ("[Medium] /problems/single-number-ii/" 137)
                             ("[Hard] /problems/copy-list-with-random-pointer/" 138)
                             ("[Medium] /problems/word-break/" 139)
                             ("[Hard] /problems/word-break-ii/" 140)
                             ("[Medium] /problems/linked-list-cycle/" 141)
                             ("[Medium] /problems/linked-list-cycle-ii/" 142)
                             ("[Medium] /problems/reorder-list/" 143)
                             ("[Medium] /problems/binary-tree-preorder-traversal/" 144)
                             ("[Hard] /problems/binary-tree-postorder-traversal/" 145)
                             ("[Hard] /problems/lru-cache/" 146)
                             ("[Medium] /problems/insertion-sort-list/" 147)
                             ("[Medium] /problems/sort-list/" 148)
                             ("[Hard] /problems/max-points-on-a-line/" 149)
                             ("[Medium] /problems/evaluate-reverse-polish-notation/" 150)
                             ("[Medium] /problems/reverse-words-in-a-string/" 151)
                             ("[Medium] /problems/maximum-product-subarray/" 152)
                             ("[Medium] /problems/find-minimum-in-rotated-sorted-array/" 153)
                             ("[Hard] /problems/find-minimum-in-rotated-sorted-array-ii/" 154)
                             ("[Easy] /problems/min-stack/" 155)
                             ("[Medium] /problems/binary-tree-upside-down/" 156)
                             ("[Easy] /problems/read-n-characters-given-read4/" 157)
                             ("[Hard] /problems/read-n-characters-given-read4-ii-call-multiple-times/" 158)
                             ("[Hard] /problems/longest-substring-with-at-most-two-distinct-characters/" 159)
                             ("[Easy] /problems/intersection-of-two-linked-lists/" 160)
                             ("[Medium] /problems/one-edit-distance/" 161)
                             ("[Medium] /problems/find-peak-element/" 162)
                             ("[Medium] /problems/missing-ranges/" 163)
                             ("[Hard] /problems/maximum-gap/" 164)
                             ("[Easy] /problems/compare-version-numbers/" 165)
                             ("[Medium] /problems/fraction-to-recurring-decimal/" 166)
                             ("[Medium] /problems/two-sum-ii-input-array-is-sorted/" 167)
                             ("[Easy] /problems/excel-sheet-column-title/" 168)
                             ("[Easy] /problems/majority-element/" 169)
                             ("[Easy] /problems/two-sum-iii-data-structure-design/" 170)
                             ("[Easy] /problems/excel-sheet-column-number/" 171)
                             ("[Easy] /problems/factorial-trailing-zeroes/" 172)
                             ("[Medium] /problems/binary-search-tree-iterator/" 173)
                             ("[Hard] /problems/dungeon-game/" 174)
                             ("[Medium] /problems/largest-number/" 179)
                             ("[Medium] /problems/reverse-words-in-a-string-ii/" 186)
                             ("[Medium] /problems/repeated-dna-sequences/" 187)
                             ("[Hard] /problems/best-time-to-buy-and-sell-stock-iv/" 188)
                             ("[Easy] /problems/rotate-array/" 189)
                             ("[Easy] /problems/reverse-bits/" 190)
                             ("[Easy] /problems/number-of-1-bits/" 191)
                             ("[Easy] /problems/house-robber/" 198)
                             ("[Medium] /problems/binary-tree-right-side-view/" 199)
                             ("[Medium] /problems/number-of-islands/" 200)
                             ("[Medium] /problems/bitwise-and-of-numbers-range/" 201)
                             ("[Easy] /problems/happy-number/" 202)
                             ("[Easy] /problems/remove-linked-list-elements/" 203)
                             ("[Easy] /problems/count-primes/" 204)
                             ("[Easy] /problems/isomorphic-strings/" 205)
                             ("[Easy] /problems/reverse-linked-list/" 206)
                             ("[Medium] /problems/course-schedule/" 207)
                             ("[Medium] /problems/implement-trie-prefix-tree/" 208)
                             ("[Medium] /problems/minimum-size-subarray-sum/" 209)
                             ("[Medium] /problems/course-schedule-ii/" 210)
                             ("[Medium] /problems/add-and-search-word-data-structure-design/" 211)
                             ("[Hard] /problems/word-search-ii/" 212)
                             ("[Medium] /problems/house-robber-ii/" 213)
                             ("[Hard] /problems/shortest-palindrome/" 214)
                             ("[Medium] /problems/kth-largest-element-in-an-array/" 215)
                             ("[Medium] /problems/combination-sum-iii/" 216)
                             ("[Easy] /problems/contains-duplicate/" 217)
                             ("[Hard] /problems/the-skyline-problem/" 218)
                             ("[Easy] /problems/contains-duplicate-ii/" 219)
                             ("[Medium] /problems/contains-duplicate-iii/" 220)
                             ("[Medium] /problems/maximal-square/" 221)
                             ("[Medium] /problems/count-complete-tree-nodes/" 222)
                             ("[Easy] /problems/rectangle-area/" 223)
                             ("[Medium] /problems/basic-calculator/" 224)
                             ("[Easy] /problems/implement-stack-using-queues/" 225)
                             ("[Easy] /problems/invert-binary-tree/" 226)
                             ("[Medium] /problems/basic-calculator-ii/" 227)
                             ("[Easy] /problems/summary-ranges/" 228)
                             ("[Medium] /problems/majority-element-ii/" 229)
                             ("[Medium] /problems/kth-smallest-element-in-a-bst/" 230)
                             ("[Easy] /problems/power-of-two/" 231)
                             ("[Easy] /problems/implement-queue-using-stacks/" 232)
                             ("[Medium] /problems/number-of-digit-one/" 233)
                             ("[Easy] /problems/palindrome-linked-list/" 234)
                             ("[Easy] /problems/lowest-common-ancestor-of-a-binary-search-tree/" 235)
                             ("[Medium] /problems/lowest-common-ancestor-of-a-binary-tree/" 236)
                             ("[Easy] /problems/delete-node-in-a-linked-list/" 237)
                             ("[Medium] /problems/product-of-array-except-self/" 238)
                             ("[Hard] /problems/sliding-window-maximum/" 239)
                             ))


(defun leet-code--fresh-request ()
  (if (eq request-backend 'url-retrieve)
      (url-cookie-clean-up)
    (when (file-exists-p (request--curl-cookie-jar))
      (delete-file (request--curl-cookie-jar))))

  (request leet-code-base-url
           :sync t
           :headers leet-code-request-header))

(defun leet-code--login (uname pwd)
  (interactive
   (list
    (read-string "input username or email ")
    (read-passwd "intpu password ")))
  
  (unless (leet-code--is-loggedin)
    (leet-code--fresh-request)
    (request (concat leet-code-base-url "/" leet-code-action-login)
             :type "POST"
             :data `(("login" . ,uname )
                     ("password" . ,pwd)
                     ("Host" . ,leet-code-base-domain)
                     (,leet-code-csrf-key .
                                          ,(leet-code--get-cookie-val
                                            leet-code-csrf-cookie-key)))
             :headers leet-code-request-header)))

(defun leet-code--is-loggedin ()
  "Detect login state by phpsessid."
  (leet-code--get-cookie-val "PHPSESSID"))


(defun leet-code--logout ()
  (interactive)
  (request (concat leet-code-base-url "/" leet-code-action-logout)
           :sync t
           :headers leet-code-request-header))


(defun* leet-code--submit
    (content &optional qname qid &key (type "large") (lang leet-code-default-lang))
  (assert (leet-code--is-loggedin) t "login first")
  (unless (and qname qid)
    (let ((quest
           (assoc
            (completing-read "choose quest: " leet-code-quests nil t)
            leet-code-quests
            )))
      (if quest
          (setq qname (car quest)  qid (cadr quest))
        (error "invalid quest"))))
  
  (request
   (concat
    leet-code-base-url
    (replace-regexp-in-string leet-code-quest-cleaner "" qname)
    "/submit/" )
   :type "POST"
   :data `((,leet-code-csrf-key .
                                ,(leet-code--get-cookie-val
                                  leet-code-csrf-cookie-key))
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
         "acectrl.init(\r?\n?\s*\\(\\[.*,\\]\\),"
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
    (when (not (stringp code-obj))
      (let ((cpp-entry            
             (car (delq nil
                        (mapcar (lambda (entry)
                                  (and
                                   (string= (cdr (assoc 'value entry)) leet-code-default-lang)
                                   entry)) code-obj)))))
        (cdr
         (assoc 'defaultCode cpp-entry))))))


(defun leet-code--parse-info (raw-content)
  "Parse html RAW-CONTENT get info."
  (let ((content-re "<meta name=\"description\" content=\"\\(\[^>\]+\\)\/>")
        (tag-re "<a.*?href=\"/tag/\\(\[^/\]*\\)/\">")
        info
        (tags "\n\ntags: "))

    (save-excursion
      (with-temp-buffer
        (insert raw-content)
        (goto-char (point-min))
        (when (re-search-forward content-re nil t)
          (setq info (match-string-no-properties 1)))
      
        (while (re-search-forward tag-re nil t)
          (setq tags
                (concat tags " " (match-string-no-properties 1))))))
    
    (concat info tags)))

(defun leet-code--display-info (qname)
  (interactive
   (list
    (completing-read
     "choose question: "
     leet-code-quests
     nil t)))
  (let* ((data
          (request-response-data
           (request
            (concat leet-code-base-url
                    (replace-regexp-in-string
                     leet-code-quest-cleaner
                     "" qname) "/")
            :parser 'buffer-string
            :headers leet-code-request-header
            :sync t
            )))
         (info (leet-code--parse-info data))
         (start (point));skip default comments
         (end (+ start (length info))))
    (save-excursion
      (goto-char start)
      (insert info)      
      (comment-region start end)
      (newline)
;      (fill-region start (point))
      (when (y-or-n-p "insert code template? ")
        (insert (leet-code--parse-code data))
        (newline)))))

(defun leet-code--show-quests ()
  "Show quests."
  (interactive)
  (completing-read
     "question: "
     leet-code-quests nil t))

;; (setq url-debug nil)
;; (setq url-proxy-services
;;       '(("http"     . "127.0.0.1:8888")
;; 	("https"      . "127.0.0.1:8888")
;; 	("no_proxy" . "^.*example.com")))

(provide 'leet-code)

;;; leet-code.el ends here
