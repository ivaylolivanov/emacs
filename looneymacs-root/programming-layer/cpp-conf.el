;;; cpp-conf --- Holds the configurations for C / C++

;;; Commentary:
;; - C/C++ configurations and packages to make work
;; with these languages easier and more pleasant (not
;; that it could be any more than it already is :-) )



;;; Code:
;;==========================
;;= C / C++ Configurations =
;;==========================
;; - Define constants and defaults
;; - Define functions
;; - Apply C/C++ style



;;=================================
;;= Define constants and defaults =
;;=================================
(defconst personal-c-style-name "Personal C style"
  "The name of the personla C/C++ coding style.")
(defconst personal-c-style
  '("user"
    (c-basic-offset                    . 4)
    (c-indent-level                    . 4)
    (tab-width                         . 4)
    (c-max-one-liner-length            . 60)
    (c-tab-always-indent               . t)
    (c-comment-only-line-offset        . 0)
    (indent-tabs-mode                  . nil)
    (c-recognize-knr-p                 . nil)
    (comment-column                    . 65)
    (c-indent-comment-alist            . ((other . (space . 4))))
    (c-indent-comments-syntactically-p . t)
    (c-argdecl-indent                  . 0)
    (backward-delete-function          . nil)
    (c-continued-statement-offset      . 4)

    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))

    (c-hanging-braces-alist
     . ((block-open          . (before after))
        (block-close         . (before after))
        (brace-list-open     . (before after))
        (brace-list-close    . (before after))
        (brace-list-intro    . (before after))
        (brace-list-entry)
        (class-open          . (before after))
        (class-close         . (after))
        (defun-open          . (before after))
        (defun-close         . (before after))
        (inline-open         . (before after))
        (inline-close        . (after))
        (statement-cont      . (after))
        (statement-case-open . (before))
        (substatement-open   . (before))
        (topmost-intro       . (before))))

    (c-hanging-colons-alist
     . ((access-label)
        (access-key)
        (case-label)
        (inher-intro)
        (label)
        (member-init-intro)))

    (c-cleanup-list
     . (defun-close-semi
         empty-defun-braces
         list-close-comma
         one-liner-defun
         scope-operator
         ))

    (c-offsets-alist
     . ((access-label          . -4)
        (annotation-top-cont   .  0)
        (annotation-var-cont   .  4)
        (arglist-close         .  c-lineup-close-paren)
        (arglist-cont          .  0)
        (arglist-cont-nonempty .  c-lineup-arglist)
        (arglist-intro         .  4)
        (block-close           .  0)
        (block-open            .  0)
        (brace-entry-open      .  0)
        (brace-list-close      .  0)
        (brace-list-entry      .  0)
        (brace-list-open       .  0)
        (brace-list-intro      .  4)
        (c                     .  c-lineup-C-comments)
        (case-label            .  4)
        (catch-clause          .  0)
        (class-close           .  0)
        (class-open            .  0)
        (comment-intro         .  c-lineup-comment)
        (composition-close     .  0)
        (composition-open      .  0)
        (cpp-define-intro         c-lineup-cpp-define +)
        (cpp-macro             . -1000)
        (cpp-macro-cont        .  4)
        (defun-block-intro     .  4)
        (defun-close           .  0)
        (defun-open            .  0)
        (do-while-closure      .  0)
        (else-clause           .  0)
        (extern-lang-close     .  0)
        (extern-lang-open      .  0)
        (friend                .  0)
        (func-decl-cont        .  4)
        (inclass               .  4)
        (incomposition         .  4)
        (inexpr-class          .  4)
        (inexpr-statement      .  4)
        (inextern-lang         .  4)
        (inher-cont            .  c-lineup-multi-inher)
        (inher-intro           .  4)
        (inlambda              .  0)
        (inline-close          .  0)
        (inline-open           .  4)
        (inmodule              .  4)
        (innamespace           .  4)
        (knr-argdecl           .  0)
        (knr-argdecl-intro     .  4)
        (label                 . -4)
        (lambda-intro-cont     .  4)
        (member-init-cont      .  c-lineup-multi-inher)
        (member-init-intro     .  4)
        (module-close          .  0)
        (module-open           .  0)
        (namespace-close       .  0)
        (namespace-open        .  0)
        (objc-method-args-cont .  c-lineup-ObjC-method-args)
        (objc-method-call-cont    c-lineup-ObjC-method-call-colons
                                  c-lineup-ObjC-method-call +)
        (objc-method-intro     .  [0])
        (statement             .  0)
        (statement-block-intro .  4)
        (statement-case-open   .  0)
        (statement-case-intro  .  4)
        (statement-cont        .  4)
        (stream-op             .  c-lineup-streamop)
        (string                . -1000)
        (substatement          .  4)
        (substatement-label    .  4)
        (substatement-open     .  0)
        (topmost-intro-cont    .  0)
        (topmost-intro         .  0)
        (topmost-intro-cont    .  0)))
    (c-echo-syntactic-information-p . t))
  "Defines my personal C/C++ coding style.")

(defconst clang-format-filename ".clang-format"
  "Clang's formatting config filename.")
(defconst clang-format-personal-c-style "
Language: Cpp
AccessModifierOffset: -4
AlignAfterOpenBracket: false
AlignArrayOfStructures: None
AlignConsecutiveAssignments: false
AlignConsecutiveBitFields: None
AlignConsecutiveDeclarations: None
AlignConsecutiveMacros: None
AlignEscapedNewlines: Left
AlignOperands: Align
AlignTrailingComments: false
AllowAllArgumentsOnNextLine: false
AllowAllParametersOfDeclarationOnNextLine: false
AllowShortBlocksOnASingleLine: Empty
AllowShortCaseLabelsOnASingleLine: false
AllowShortEnumsOnASingleLine: false
AllowShortFunctionsOnASingleLine: All
AllowShortIfStatementsOnASingleLine: Never
AllowShortLambdasOnASingleLine: All
AllowShortLoopsOnASingleLine: false
AlwaysBreakBeforeMultilineStrings: false
BinPackArguments: true
BinPackParameters: false
BitFieldColonSpacing: After
BraceWrapping:
  AfterCaseLabel: true
  AfterClass: true
  AfterControlStatement: Never
  AfterEnum: true
  AfterFunction: true
  AfterNamespace: true
  AfterStruct: true
  AfterUnion: true
  AfterExternBlock: true
  BeforeCatch: true
  BeforeElse: true
  BeforeLambdaBody: true
  BeforeWhile: false
  IndentBraces: false
  SplitEmptyFunction: false
  SplitEmptyRecord: true
  SplitEmptyNamespace: true
BreakBeforeBinaryOperators: true
BreakBeforeBraces: Custom
BreakBeforeConceptDeclarations: true
BreakBeforeTernaryOperators: false
BreakConstructorInitializers: AfterColon
BreakInheritanceList: AfterComma
BreakStringLiterals: true
ColumnLimit: 80
CompactNamespaces: false
ConstructorInitializerIndentWidth: 4
ContinuationIndentWidth: 4
Cpp11BracedListStyle: false
DerivePointerAlignment: true
DisableFormat: false
EmptyLineAfterAccessModifier: Leave
EmptyLineBeforeAccessModifier: Leave
FixNamespaceComments: true
IncludeBlocks: Preserve
IndentAccessModifiers: false
IndentCaseBlocks: false
IndentCaseLabels: true
IndentExternBlock: Indent
IndentGotoLabels: false
IndentPPDirectives: AfterHash
IndentWidth: 4
IndentWrappedFunctionNames: true
InsertTrailingCommas: None
LambdaBodyIndentation: Signature
MaxEmptyLinesToKeep: 1
NamespaceIndentation: None
PPIndentWidth: -1
PackConstructorInitializers: NextLine
PenaltyBreakAssignment: 0
PenaltyBreakBeforeFirstCallParameter: 0
PenaltyBreakComment: 0
PenaltyBreakFirstLessLess: 0
PenaltyBreakOpenParenthesis: 0
PenaltyBreakString: 0
PenaltyBreakTemplateDeclaration: 0
PenaltyExcessCharacter: 0
PenaltyIndentedWhitespace: 0
PenaltyReturnTypeOnItsOwnLine: 0
PointerAlignment: Left
QualifierAlignment: Left
ReferenceAlignment: Left
ReflowComments: true
RemoveBracesLLVM: false
SeparateDefinitionBlocks: Always
ShortNamespaceLines: 1
SortIncludes: CaseInsensitive
SortUsingDeclarations: true
SpaceAfterCStyleCast: false
SpaceAfterLogicalNot: false
SpaceAfterTemplateKeyword: false
SpaceAroundPointerQualifiers: Default
SpaceBeforeAssignmentOperators: true
SpaceBeforeCaseColon: false
SpaceBeforeCpp11BracedList: true
SpaceBeforeCtorInitializerColon: false
SpaceBeforeInheritanceColon: false
SpaceBeforeParens: Custom
SpaceBeforeParensOptions:
  AfterControlStatements: true
  AfterForeachMacros: true
  AfterFunctionDeclarationName: false
  AfterFunctionDefinitionName: false
  AfterIfMacros: true
  AfterOverloadedOperator: false
  BeforeNonEmptyParentheses: false
SpaceBeforeRangeBasedForLoopColon: true
SpaceBeforeSquareBrackets: false
SpaceInEmptyBlock: true
SpacesBeforeTrailingComments: 4
SpacesInAngles: Never
SpacesInConditionalStatement: false
SpacesInLineCommentPrefix:
  Minimum: 1
  Maximum: -1
SpacesInSquareBrackets: false
Standard: Auto
TabWidth: 4
UseTab: Never"
  "Defines my personal C/C++ coding style in clang format.")

(setq c-default-style `((awk-mode  . "awk")
                        (c-mode    . ,personal-c-style-name)
                        (c++-mode  . ,personal-c-style-name)
                        (java-mode . "java")
                        (other     . "gnu")))



;;====================
;;= Define functions =
;;====================
(defun apply-c-style()
  "Apply personal C/C++ style.
Meant for hooking to 'c-mode-hook' and 'c++-mode-hook'."
  (c-set-style personal-c-style-name))

(defun install-personal-clang-format()
  "Install clang-format in the current project's root if it does not exist."
  (interactive)
  (setq current-project-root (project-root (project-current)))
  (setq clang-format-filepath
        (expand-file-name clang-format-filename current-project-root))
  (if (file-exists-p clang-format-filepath)
      (message "There is already a .clang-format file in %s. %s!"
               current-project-root
               "Please remove it if you want the default one")
    (with-temp-file clang-format-filepath
      (insert clang-format-personal-c-style))))
;;====================


;;=====================
;;= Apply C/C++ style =
;;=====================
(c-add-style personal-c-style-name personal-c-style)
(add-hook 'c-mode-hook   'apply-c-style)
(add-hook 'c++-mode-hook 'apply-c-style)
;;=====================

(provide 'cpp-conf)
;;; cpp-conf.el ends here
