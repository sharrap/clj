(in-package :clj.parser.nfa)

(defrule Literal
  (IntegerLiteral)
  (FloatingPointLiteral)
  (BooleanLiteral)
  (|charlit|)
  (|strlit|)
  (|null|))

(defrule IntegerLiteral
  (|intlit|)
  (|longlit|))

(defrule FloatingPointLiteral
  (|floatlit|)
  (|doublelit|))

(defrule BooleanLiteral
  (|true|)
  (|false|))

(defrule Type
  (PrimitiveType)
  (ReferenceType))

(defrule PrimitiveType
  ({Annotation} NumericType)
  ({Annotation} |boolean|))

(defrule NumericType
  (IntegralType)
  (FloatingPointType))

(defrule IntegralType
  (|byte|)
  (|short|)
  (|int|)
  (|long|)
  (|char|))

(defrule FloatingPointType
  (|float|)
  (|double|))

(defrule ReferenceType
  (ClassOrInterfaceType)
  (TypeVariable)
  (ArrayType))

(defrule ClassOrInterfaceType
  (ClassType)
  (InterfaceType))

(defrule ClassType
  ({Annotation} |identifier| [TypeArguments])
  (ClassOrInterfaceType |dot| {Annotation} |identifier| [TypeArguments]))

(defrule InterfaceType
  (ClassType))

(defrule TypeVariable
  ({Annotation} |identifier|))

(defrule ArrayType
  (PrimitiveType Dims)
  (ClassOrInterfaceType Dims)
  (TypeVariable Dims))

(defrule Dims
  ({Annotation} |lbrack| |rbrack| {{Annotation} |lbrack| |rbrack|}))

(defrule TypeParameter
  ({TypeParameterModifier} |identifier| [TypeBound]))

(defrule TypeParameterModifier
  (Annotation))

(defrule TypeBound
  (|extends| TypeVariable)
  (|extends| ClassOrInterfaceType {AdditionalBound}))

(defrule AdditionalBound
  (|and| InterfaceType))

(defrule TypeArguments
  (|lt| TypeArgumentList |gt|))

(defrule TypeArgumentList
  (TypeArgument {|comma| TypeArgument}))

(defrule TypeArgument
  (ReferenceType)
  (Wildcard))

(defrule Wildcard
  ({Annotation} |question| [WildcardBounds]))

(defrule WildcardBounds
  (|extends| ReferenceType)
  (|super| ReferenceType))

(defrule TypeName
  (|identifier|)
  (PackageOrTypeName |dot| |identifier|))

(defrule PackageOrTypeName
  (|identifier|)
  (PackageOrTypeName |dot| |identifier|))

(defrule ExpressionName
  (|identifier|)
  (AmbiguousName |dot| |identifier|))

(defrule MethodName
  (|identifier|))

(defrule PackageName
  (|identifier|)
  (PackageName |dot| |identifier|))

(defrule AmbiguousName
  (|identifier|)
  (AmbiguousName |dot| |identifier|))

(defrule CompilationUnit
  ([PackageDeclaration] {ImportDeclaration} {TypeDeclaration}))

(defrule PackageDeclaration
  ({PackageModifier} |package| |identifier| {|dot| |identifier|} |semi|))

(defrule PackageModifier
  (Annotation))

(defrule ImportDeclaration
  (SingleTypeImportDeclaration)
  (TypeImportOnDemandDeclaration)
  (SingleStaticImportDeclaration)
  (StaticImportOnDemandDeclaration))

(defrule SingleTypeImportDeclaration
  (|import| TypeName |semi|))

(defrule TypeImportOnDemandDeclaration
  (|import| PackageOrTypeName |dot| |times| |semi|))

(defrule SingleStaticImportDeclaration
  (|import| |static| TypeName |dot| |identifier| |semi|))

(defrule StaticImportOnDemandDeclaration
  (|import| |static| TypeName |dot| |times| |semi|))

(defrule TypeDeclaration
  (ClassDeclaration)
  (InterfaceDeclaration)
  (|semi|))

(defrule ClassDeclaration
  (NormalClassDeclaration)
  (EnumDeclaration))

(defrule NormalClassDeclaration
  ({ClassModifier} |class| |identifier| [TypeParameters] [Superclass]
   [Superinterfaces] ClassBody))

(defrule ClassModifier
  (Annotation)
  (|public|)
  (|protected|)
  (|private|)
  (|abstract|)
  (|static|)
  (|final|)
  (|strictfp|))

(defrule TypeParameters
  (|lt| TypeParameterList |gt|))

(defrule Superclass
  (|extends| ClassType))

(defrule Superinterfaces
  (|implements| InterfaceTypeList))

(defrule InterfaceTypeList
  (InterfaceType {|comma| InterfaceType}))

(defrule ClassBody
  (|lbrace| {ClassBodyDeclaration} |rbrace|))

(defrule ClassBodyDeclaration
  (ClassMemberDeclaration)
  (InstanceInitializer)
  (StaticInitializer)
  (ConstructorDeclaration))

(defrule ClassMemberDeclaration
  (FieldDeclaration)
  (MethodDeclaration)
  (ClassDeclaration)
  (InterfaceDeclaration)
  (|semi|))

(defrule FieldDeclaration
  ({FieldModifier} UnannType VariableDeclaratorList |semi|))

(defrule FieldModifier
  (Annotation)
  (|public|)
  (|protected|)
  (|private|)
  (|static|)
  (|final|)
  (|transient|)
  (|volatile|))

(defrule VariableDeclaratorList
  (VariableDeclarator {|comma| VariableDeclarator}))

(defrule VariableDeclarator
  (VariableDeclaratorId [|assign| VariableInitializer]))

(defrule VariableDeclaratorId
  (|identifier| [Dims]))

(defrule VariableInitializer
  (Expression)
  (ArrayInitializer))

(defrule UnannType
  (UnannPrimitiveType)
  (UnannReferenceType))

(defrule UnannPrimitiveType
  (NumericType)
  (|boolean|))

(defrule UnannReferenceType
  (UnannClassOrInterfaceType)
  (UnannTypeVariable)
  (UnannArrayType))

(defrule UnannClassOrInterfaceType
  (UnannClassType)
  (UnannInterfaceType))

(defrule UnannClassType
  (|identifier| [TypeArguments])
  (UnannClassOrInterfaceType |dot| {Annotation} |identifier| [TypeArguments]))

(defrule UnannInterfaceType
  (UnannClassType))

(defrule UnannTypeVariable
  (|identifier|))

(defrule UnannArrayType
  (UnannPrimitiveType Dims)
  (UnannClassOrInterfaceType Dims)
  (UnannTypeVariable Dims))

(defrule MethodDeclaration
  ({MethodModifier} MethodHeader MethodBody))

(defrule MethodModifier
  (Annotation)
  (|public|)
  (|protected|)
  (|private|)
  (|abstract|)
  (|static|)
  (|final|)
  (|synchronized|)
  (|native|)
  (|strictfp|))

(defrule MethodHeader
  (Result MethodDeclarator [Throws])
  (TypeParameters {Annotation} Result MethodDeclarator [Throws]))

(defrule Result
  (UnannType)
  (|void|))

(defrule MethodDeclarator
  (|identifier| |lparen| [FormalParameterList] |rparen| [Dims]))

(defrule FormalParameterList
  (ReceiverParameter)
  (FormalParameters |comma| LastFormalParameter)
  (LastFormalParameter))

(defrule FormalParameters
  (FormalParameter {|comma| FormalParameter})
  (ReceiverParameter {|comma| FormalParameter))

(defrule FormalParameter
  ({VariableModifier} UnannType VariableDeclaratorId))

(defrule VariableModifier
  (Annotation)
  (|final|))

(defrule LastFormalParameter
  ({VariableModifier} UnannType {Annotation} |threedots| VariableDeclaratorId)
  (FormalParameter))

(defrule ReceiverParameter
  ({Annotation} UnannType [|identifier| |dot|] |this|))

(defrule Throws
  (|throws| ExceptionTypeList))

(defrule ExceptionTypeList
  (ExceptionType {|comma| ExceptionType}))

(defrule ExceptionType
  (ClassType)
  (TypeVariable))

(defrule MethodBody
  (Block)
  (|semi|))

(defrule InstanceInitializer
  (Block))

(defrule StaticInitializer
  (|static| Block))

(defrule ConstructorDeclaration
  ({ConstructorModifier} ConstructorDeclarator [Throws] ConstructorBody))

(defrule ConstructorModifier
  (Annotation)
  (|public|)
  (|protected|)
  (|private|))

(defrule ConstructorDeclarator
  ([TypeParameters] SimpleTypeName |lparen| [FormalParameterList] |rparen|))

(defrule SimpleTypeName
  (|identifier|))

(defrule ConstructorBody
  (|lparen| [ExplicitConstructorInvocation] [BlockStatements] |rparen|))

(defrule ExplicitConstructorInvocation
  ([TypeArguments] |this| |lparen| [ArgumentList] |rparen| |semi|)
  ([TypeArguments] |super| |lparen| [ArgumentList] |rparen| |semi|)
  (ExpressionName |dot| [TypeArguments] |super| |lparen| [ArgumentList] |rparen| |semi|)
  (Primary |dot| [TypeArguments] |super| |lparen| [ArgumentList] |rparen| |semi|))

(defrule EnumDeclaration
  ({ClassModifier} |enum| |identifier| [Superinterfaces] EnumBody))

(defrule EnumBody
  (|lbrace| [EnumConstantList] [|comma|] [EnumBodyDeclarations] |rbrace|))

(defrule EnumConstantList
  (EnumConstant {|comma| EnumConstant}))

(defrule EnumConstant
  ({EnumConstantModifier} |identifier| [|lparen| [ArgumentList] |rparen|]
   [ClassBody]))

(defrule EnumConstantModifier
  (Annotation))

(defrule EnumBodyDeclarations
  (|semi| {ClassBodyDeclaration}))

(defrule InterfaceDeclaration
  (NormalInterfaceDeclaration)
  (AnnotationTypeDeclaration))

(defrule NormalInterfaceDeclaration
  ({InterfaceModifier} |interface| |identifier| [TypeParameters] [ExtendsInterfaces] InterfaceBody))

(defrule InterfaceModifier
  (Annotation)
  (|public|)
  (|protected|)
  (|private|)
  (|abstract|)
  (|static|)
  (|strictfp|))

(defrule ExtendsInterface
  (|extends| InterfaceTypeList))

(defrule InterfaceBody
  (|lbrace| {InterfaceMemberDeclaration} |rbrace|))

(defrule InterfaceMemberDeclaration
  (ConstantDeclaration)
  (InterfaceMethodDeclaration)
  (ClassDeclaration)
  (InterfaceDeclaration)
  (|semi|))

(defrule ConstantDeclaration
  ({ConstantModifier} UnannType VariableDeclaratorList |semi|))

(defrule ConstantModifier
  (Annotation)
  (|public|)
  (|static|)
  (|final|))

(defrule InterfaceMethodModifier
  (Annotation)
  (|public|)
  (|abstract|)
  (|default|)
  (|static|)
  (|strictfp|))

(defrule AnnotationTypeDeclaration
  ({InterfaceModifier} |at| |interface| |identifier| AnnotationTypeBody))

(defrule AnnotationTypeBody
  (|lbrace| {AnnotationTypeMemberDeclaration} |rbrace|))

(defrule AnnotationTypeMemberDeclaration
  (AnnotationTypeElementDeclaration)
  (ConstantDeclaration)
  (ClassDeclaration)
  (InterfaceDeclaration)
  (|semi|))

(defrule AnnotationTypeElementDeclaration
  ({AnnotationTypeElementModifier} UnannType |identifier| |lparen| |rparen|
   [Dims] [DefaultValue] |semi|))

(defrule AnnotationTypeElementModifier
  (Annotation)
  (|public|)
  (|abstract|))

(defrule DefaultValue
  (|default| ElementValue))

(defrule Annotation
  (NormalAnnotation)
  (MarkerAnnotation)
  (SingleElementAnnotation))

(defrule NormalAnnotation
  (|at| TypeName |lparen| [ElementValuePairList] |rparen|))

(defrule ElementValuePairList
  (ElementValuePair {|comma| ElementValuePair}))

(defrule ElementValuePair
  (|identifier| |assign| ElementValue))

(defrule ElementValue
  (ConditionalExpression)
  (ElementValueArrayInitializer)
  (Annotation))

(defrule ElementValueArrayInitializer
  (|lbrace| [ElementValueList] [|comma|] |rbrace|))

(defrule ElementValueList
  (ElementValue {|comma| ElementValue}))

(defrule MarkerAnnotation
  (|at| TypeName))

(defrule SingleElementAnnotation
  (|at| TypeName |lparen| ElementValue |rparen|))

(defrule ArrayInitializer
  (|lbrace| [VariableInitializerList] [|comma|] |rbrace|))

(defrule VariableInitializerList
  (VariableInitializer {|comma| VariableInitializer}))

(defrule Block
  (|lbrace| [BlockStatements] |rbrace|))

(defrule BlockStatements
  (BlockStatement {BlockStatement}))

(defrule BlockStatement
  (LocalVariableDeclarationStatement)
  (ClassDeclaration)
  (Statement))

(defrule LocalVariableDeclarationStatement
  (LocalVariableDeclaration |semi|))

(defrule LocalVariableDeclaration
  ({VariableModifier} UnannType VariableDeclaratorList))

(defrule Statement
  (StatementWithoutTrailingSubstatement)
  (LabeledStatement)
  (IfThenStatement)
  (IfThenElseStatement)
  (WhileStatement)
  (ForStatement))

(defrule StatementNoShortIf
  (StatementWithoutTrailingSubstatement)
  (LabeledStatementNoShortIf)
  (IfThenElseStatementNoShortIf)
  (WhileStatementNoShortIf)
  (ForStatementNoShortIf))

(defrule StatementWithoutTrailingSubstatement
  (Block)
  (EmptyStatement)
  (ExpressionStatement)
  (AssertStatement)
  (SwitchStatement)
  (DoStatement)
  (BreakStatement)
  (ContinueStatement)
  (ReturnStatement)
  (SynchronizedStatement)
  (ThrowStatement)
  (TryStatement))

(defrule EmptyStatement
  (|semi|))

(defrule LabeledStatement
  (|identifier| |colon| Statement))

(defrule LabeledStatementNoShortIf
  (|identifier| |colon| StatementNoShortIf))

(defrule ExpressionStatement
  (StatementExpression |semi|))

(defrule StatementExpression
  (Assignment)
  (PreIncrementExpression)
  (PreDecrementExpression)
  (PostIncrementExpression)
  (PostDecrementExpression)
  (MethodInvocation)
  (ClassInstanceCreationExpression))

(defrule IfThenStatement
  (|if| |lparen| Expression |rparen| Statement))

(defrule IfThenElseStatement
  (|if| |lparen| Expression |rparen| StatementNoShortIf |else| Statement))

(defrule IfThenElseStatementNoShortIf
  (|if| |lparen| Expression |rparen| StatementNoShortIf |else| StatementNoShortIf))

(defrule AssertStatement
  (|assert| Expression |semi|)
  (|assert| Expression |colon| Expression |semi|))

(defrule SwitchStatement
  (|switch| |lparen| Expression |rparen| SwitchBlock))

(defrule SwitchBlock
  (|lbrace| {SwitchBlockStatementGroup} {SwitchLabel} |rbrace|))

(defrule SwitchBlockStatementGroup
  (SwitchLabels BlockStatements))

(defrule SwitchLabels
  (SwitchLabel {SwitchLabels}))

(defrule SwitchLabel
  (|case| ConstantExpression |colon|)
  (|case| EnumConstantName |colon|)
  (|default| |colon|))

(defrule EnumConstantName
  (|identifier|))

(defrule WhileStatement
  (|while| |lparen| Expression |rparen| Statement))

(defrule WhileStatementNoShortIf
  (|while| |lparen| Expression |rparen| StatementNoShortIf))

(defrule DoStatement
  (|do| Statement |while| |lparen| Expression |rparen| |semi|))

(defrule ForStatement
  (BasicForStatement)
  (EnhancedForStatement))

(defrule ForStatementNoShortIf
  (BasicForStatementNoShortIf)
  (EnhancedForStatementNoShortIf))

(defrule BasicForStatement
  (|for| |lparen| [ForInit] |semi| [Expression] |semi| [ForUpdate] |rparen|
   Statement))

(defrule BasicForStatementNoShortIf
  (|for| |lparen| [ForInit] |semi| [Expression] |semi| [ForUpdate] |rparen|
   StatementNoShortIf))

(defrule ForInit
  (StatementExpressionList)
  (LocalVariableDeclaration))

(defrule ForUpdate
  (StatementExpressionList))

(defrule StatementExpressionList
  (StatementExpression {|comma| StatementExpression}))

(defrule EnhancedForStatement
  (|for| |lparen| {VariableModifier} UnannType VariableDeclaratorId
   |colon| Expression |rparen| Statement))

(defrule EnhancedForStatementNoShortIf
  (|for| |lparen| {VariableModifier} UnannType VariableDeclaratorId
   |colon| Expression |rparen| StatementNoShortIf))

(defrule BreakStatement
  (|break| [|identifier|] |semi|))

(defrule ContinueStatement
  (|continue| [|identifier|] |semi|))

(defrule ReturnStatement
  (|return| [Expression] |semi|))

(defrule ThrowStatement
  (|throw| Expression |semi|))

(defrule SynchronizedStatement
  (|synchronized| |lparen| Expression |rparen| Block))

(defrule TryStatement
  (|try| Block Catches)
  (|try| Block [Catches] Finally)
  (TryWithResourcesStatement))

(defrule Catches
  (|catch| |lparen| CatchFormalParameter |rparen| Block))

(defrule CatchFormalParameter
  ({VariableModifier} CatchType VariableDeclaratorId))

(defrule CatchType
  (UnannClassType {|or| ClassType}))

(defrule Finally
  (|finally| Block))

(defrule TryWithResourcesStatement
  (|try| ResourceSpecification Block [Catches] [Finally]))

(defrule ResourceSpecification
  (|lparen| ResourceList [|semi|] |rparen|))

(defrule ResourceList
  (Resource {|semi| Resource}))

(defrule Resource
  ({VariableModifier} UnannType VariableDeclaratorId |assign| Expression))

(defrule Primary
  (PrimaryNoNewArray)
  (ArrayCreationExpression))

(defrule PrimaryNoNewArray
  (Literal)
  (ClassLiteral)
  (|this|)
  (TypeName |dot| |this|)
  (|lparen| Expression |rparen|)
  (ClassInstanceCreationExpression)
  (FieldAccess)
  (ArrayAccess)
  (MethodInvocation)
  (MethodReference))

(defrule ClassLiteral
  (TypeName {|lbrack| |rbrack|} |dot| |class|)
  (NumericType {|lbrack| |rbrack|} |dot| |class|)
  (|boolean| {|lbrack| |rbrack|} |dot| |class|)
  (|void| |dot| |class|))

(defrule ClassInstanceCreationExpression
  (UnqualifiedClassInstanceCreationExpression)
  (ExpressionName |dot| UnqualifiedClassInstanceCreationExpression)
  (Primary |dot| UnqualifiedClassInstanceCreationExpression))

(defrule UnqualifiedClassInstanceCreationExpression
  (|new| [TypeArguments] ClassOrInterfaceTypeToInstantiate
   |lparen| [ArgumentList] |rparen| [ClassBody]))

(defrule ClassOrInterfaceTypeToInstantiate
  ({Annotation} |identifier| {|dot| {Annotation} |identifier|}
   [TypeArgumentsOrDiamond]))

(defrule TypeArgumentsOrDiamond
  (TypeArguments)
  (|lt| |gt|))

(defrule FieldAccess
  (Primary |dot| |identifier|)
  (|super| |dot| |identifier|)
  (TypeName |dot| |super| |dot| |identifier|))

(defrule ArrayAccess
  (ExpressionName |lbrack| Expression |rbrack|)
  (PrimaryNoNewArray |lbrack| Expression |rbrack|))

(defrule MethodInvocation
  (MethodName |lparen| [ArgumentList] |rparen|)
  (TypeName |dot| [TypeArguments] |identifier| |lparen| [ArgumentList] |rparen|)
  (ExpressionName |dot| [TypeArguments] |identifier| |lparen| [ArgumentList]
   |rparen|)
  (Primary |dot| [TypeArguments] |identifier| |lparen| [ArgumentList] |rparen|)
  (|super| |dot| [TypeArguments] |identifier| |lparen| [ArgumentList] |rparen|)
  (TypeName |dot| |super| |dot| [TypeArguments] |identifier| |lparen|
   [ArgumentList] |rparen|))

(defrule ArgumentList
  (Expression {|comma| Expression}))

(defrule MethodReference
  (ExpressionName |twocolon| [TypeArguments] |identifier|)
  (ReferenceType |twocolon| [TypeArguments] |identifier|)
  (Primary |twocolon| [TypeArguments] |identifier|)
  (|super| |twocolon| [TypeArguments] |identifier|)
  (TypeName |dot| |super| |twocolon| [TypeArguments] |identifier|)
  (ClassType |twocolon| [TypeArguments] |new|)
  (ArrayType |twocolon| |new|))

(defrule ArrayCreationExpression
  (|new| PrimitiveType DimExprs [Dims])
  (|new| ClassORInterfaceType DimExprs [Dims])
  (|new| PrimitiveType Dims ArrayInitializer)
  (|new| ClassOrInterfaceType Dims ArrayInitializer))

(defrule DimExprs
  (DimExpr {DimExpr}))

(defrule DimExpr
  ({Annotation} |lbrack| Expression |rbrack|))

(defrule Expression
  (LambdaExpression)
  (AssignmentExpression))

(defrule LambdaExpression
  (LambdaParameters |rarrow| LambdaBody))

(defrule LambdaParameters
  (|identifier|)
  (|lparen| [FormalParameterList] |rparen|)
  (|lparen| InferredParameterList |rparen|))

(defrule LambdaBody
  (Expression)
  (Block))

(defrule AssignmentExpression
  (ConditionalExpression)
  (Assignment))

(defrule Assignment
  (LeftHandSide AssignmentOperator Expression))

(defrule LeftHandSide
  (ExpressionName)
  (FieldAccess)
  (ArrayAccess))

(defrule AssignmentOperator
  (|assign|)
  (|timesassign|)
  (|divassign|)
  (|modassign|)
  (|plusassign|)
  (|minusassign|)
  (|lshiftassign|)
  (|rshiftassign|)
  (|urshiftassign|)
  (|andassign|)
  (|xorassign|)
  (|orassign|))

(defrule ConditionalExpression
  (ConditionalOrExpression)
  (ConditionalOrExpression |question| Expression |colon| ConditionalExpression)
  (ConditionalOrExpression |question| Expression |colon| LambdaExpression))

(defrule ConditionalOrExpression
  (ConditionalAndExpression)
  (ConditionalOrExpression |boolor| ConditionalAndExpression))

(defrule ConditionalAndExpression
  (InclusiveOrExpression)
  (ConditionalAndExpression |booland| InclusiveOrExpression))

(defrule InclusiveOrExpression
  (ExclusiveOrExpression)
  (InclusiveOrExpression |or| ExclusiveOrExpression))

(defrule ExclusiveOrExpression
  (AndExpression)
  (ExclusiveOrExpression |xor| AndExpression))

(defrule AndExpression
  (EqualityExpression)
  (AndExpression |and| EqualityExpression))

(defrule EqualityExpression
  (RelationalExpression)
  (EqualityExpression |eq| RelationalExpression)
  (EqualityExpression |noteq| RelationalExpression))

(defrule RelationalExpression
  (ShiftExpression)
  (RelationalExpression |lt| ShiftExpression)
  (RelationalExpression |gt| ShiftExpression)
  (RelationalExpression |leq| ShiftExpression)
  (RelationalExpression |geq| ShiftExpression)
  (RelationalExpression |instanceof| ShiftExpression))

(defrule ShiftExpression
  (AdditiveExpression)
  (ShiftExpression |lshift| AdditiveExpression)
  (ShiftExpression |rshift| AdditiveExpression)
  (ShiftExpression |urshift| AdditiveExpression))

(defrule AdditiveExpression
  (MultiplicativeExpression)
  (AdditiveExpression |plus| MultiplicativeExpression)
  (AdditiveExpression |minus| MultiplicativeExpression))

(defrule MultiplicativeExpression
  (UnaryExpression)
  (MultiplicativeExpression |times| UnaryExpression)
  (MultiplicativeExpression |div| UnaryExpression)
  (MultiplicativeExpression |mod| UnaryExpression))

(defrule UnaryExpression
  (PreIncrementExpression)
  (PreDecrementExpression)
  (|plus| UnaryExpression)
  (|minus| UnaryExpression)
  (UnaryExpressionNotPlusMinus))

(defrule PreIncrementExpression
  (|incr| UnaryExpression))

(defrule PreDecrementExpression
  (|decr| UnaryExpression))

(defrule UnaryExpressionNotPlusMinus
  (PostfixExpression)
  (|minus| UnaryExpression)
  (|not| UnaryExpression)
  (CastExpression))

(defrule PostfixExpression
  (Primary)
  (ExpressionName)
  (PostIncrementExpression)
  (PostDecrementExpression))

(defrule PostIncrementExpression
  (PostfixExpression |incr|))

(defrule PostDecrementExpression
  (PostfixExpresion |decr|))

(defrule CastExpression
  (|lparen| PrimitiveType |rparen| UnaryExpression)
  (|lparen| ReferenceType {AdditionalBound} |rparen|
   UnaryExpressionNotPlusMinus)
  (|lparen| ReferenceType {AdditionalBound} |rparen| LambdaExpression))

(defrule ConstantExpression
  (Expression))
