(in-package :clj.ast)

;TODO:
;http://docs.oracle.com/javase/specs/jls/se8/html/jls-19.html
;Sections:
; 3
; 4
; 6
; 9
; 10
; 14
; 15

;Section 7
(defclass ASTNode ()
  ((checked-type :accessor ast-checked-type :initform nil :initarg :checked-type)))

(defclass CompilationUnitNode (ASTNode)
  ((package   :accessor ast-package   :initform nil :initarg :package)     ;Identifier or nil
   (imports   :accessor ast-imports   :initform nil :initarg :imports)     ;List of ImportNodes
   (typedecls :accessor ast-typedecls :initform nil :initarg :typedecls))) ;List of TypeDeclarationNodes

(defclass ImportNode (ASTNode) ())

(defclass StaticImportNode (ImportNode)
  ((name :accessor ast-name :initform nil :initarg :name))) ; Identifier

(defclass StaticImportOnDemandNode (ImportNode)
  ((path :accessor ast-path :initform nil :initarg :path))) ;Identifier

(defclass TypeImportNode (ImportNode)
  ((name :accessor ast-name :initform nil :initarg :name))) ;Identifier

(defclass TypeImportOnDemandNode (ImportNode)
  ((path :accessor ast-path :initform nil :initarg :path))) ;Identifier

(defclass TypeDeclarationNode (ASTNode)
  ((modifiers :accessor ast-modifiers :initform nil :initarg :modifiers) ;List of ModifierNodes ???
   (name      :accessor ast-name      :initform nil :initarg :name)      ;Identifier
   (body      :accessor ast-body      :initform nil :initarg :body)))    ;List of ClassDeclarationNode, InterfaceDeclarationNode, FieldDeclarationNode, MethodDeclarationNode, InstanceInitializerNode, StaticInitializerNode, ConstructorDeclarationNode

(defclass AnnotationDeclarationNode (TypeDeclarationNode) ())

(defclass EnumDeclarationNode (TypeDeclarationNode)
  ((constants       :accessor ast-constants       :initform nil :initarg :constants)         ;List of EnumConstantNodes
   (superinterfaces :accessor ast-superinterfaces :initform nil :initarg :superinterfaces))) ;List of Identifiers

(defclass InterfaceDeclarationNode (TypeDeclarationNode)
  ((typeparameters  :accessor ast-typeparameters  :initform nil :initarg :typeparameters)    ;List of (TypeParameterNodes???)
   (superinterfaces :accessor ast-superinterfaces :initform nil :initarg :superinterfaces))) ;List of Identifiers

(defclass ClassDeclarationNode (TypeDeclarationNode)
  ((typeparameters  :accessor ast-typeparameters  :initform nil :initarg :typeparameters)    ;List of (TypeParameterNodes???)
   (superclass      :accessor ast-superclass      :initform nil :initarg :superclass)        ;Identifier or nil
   (superinterfaces :accessor ast-superinterfaces :initform nil :initarg :superinterfaces))) ;List of Identifiers

;Section 8

(defclass EnumConstantNode (ASTNode)
  ((modifiers :accessor ast-modifiers :initform nil :initarg :modifiers) ;List of ModifierNode ???
   (name      :accessor ast-name      :initform nil :initarg :name)      ;Identifier
   (arguments :accessor ast-arguments :initform nil :initarg :arguments) ;List of ExpressionNodes ???
   (body      :accessor ast-body      :initform nil :initarg :body)      ;See body from TypeDeclarationNode

(defclass FieldDeclarationNode (ASTNode)
  ((modifiers    :accessor ast-modifiers    :initform nil :initarg :modifiers)      ;List of ModifierNodes ???
   (type         :accessor ast-type         :initform nil :initarg :type)           ;TypeNode
   (declarations :accessor ast-declarations :initform nil :initarg :declarations))) ;List of VariableDeclarationNodes

(defclass VariableDeclarationNode (ASTNode)
  ((name        :accessor ast-name        :initform nil :initarg :name)          ;Identifier
   (dims        :accessor ast-dims        :initform nil :initarg :dims)          ;DimsNode (includes annotations)
   (initializer :accessor ast-initializer :initform nil :initarg :initializer))) ;List of ExpressionNodes and ArrayInitializerNodes

(defclass MethodDeclarationNode (ASTNode)
  ((modifiers  :accessor ast-modifiers  :initform nil :initarg :modifiers)  ;List of ModifierNodes ???
   (type       :accessor ast-type       :initform nil :initarg :type)       ;TypeNode or nil (void)
   (name       :accessor ast-name       :initform nil :initarg :name)       ;Identifier
   (parameters :accessor ast-parameters :initform nil :initarg :parameters) ;List of ParameterNodes
   (throws     :accessor ast-throws     :initform nil :initarg :throws)     ;Identifier or nil
   (body       :accessor ast-body       :initform nil :initarg :body)))     ;BlockNode

(defclass ConstructorDeclarationNode (ASTNode)
  ((modifiers      :accessor ast-modifiers      :initform nil :initarg :modifiers)      ;List of ModifierNodes ???
   (typeparameters :accessor ast-typeparameters :initform nil :initarg :typeparameters) ;List of (TypeParameterNodes???)
   (name           :accessor ast-name           :initform nil :initarg :name)           ;Identifier
   (throws         :accessor ast-throws         :initform nil :initarg :throws)         ;Identifier or nil
   (body           :accessor ast-body           :initform nil :initarg :body)           ;BlockNode
   (parameters     :accessor ast-parameters     :initform nil :initarg :parameters)))   ;List of ParameterNodes

(defclass InstanceInitializerNode (ASTNode)
  ((block :accessor ast-block :initform nil :initarg :block))) ;BlockNode

(defclass StaticInitializerNode (ASTNode)
  ((block :accessor ast-block :initform nil :initarg :block))) ;BlockNode
