(in-package :clj.parser.nfa)

(defrule Literal
  (IntegerLiteral)
  (FloatingPointLiteral)
  (BooleanLiteral)
  (CharacterLiteral)
  (StringLiteral)
  (NullLiteral))

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
  ({Annotation} Identifier [TypeArguments])
  (ClassOrInterfaceType |dot| {Annotation} Identifier [TypeArguments]))

(defrule InterfaceType
  (ClassType))

(defrule TypeVariable
  ({Annotation} Identifier))

(defrule ArrayType
  (PrimitiveType Dims)
  (ClassOrInterfaceType Dims)
  (TypeVariable Dims))

(defrule Dims
  ({Annotation} |lbrack| |rbrack| {{Annotation} |lbrack| |rbrack|}))

(defrule TypeParameter
  ({TypeParameterModifier} Identifier [TypeBound]))

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
  (Identifier)
  (PackageOrTypeName |dot| Identifier))

(defrule PackageOrTypeName
  (Identifier)
  (PackageOrTypeName |dot| Identifier))

(defrule ExpressionName
  (Identifier)
  (AmbiguousName |dot| Identifier))

(defrule MethodName
  (Identifier))

(defrule PackageName
  (Identifier)
  (PackageName |dot| Identifier))

(defrule AmbiguousName
  (Identifier)
  (AmbiguousName |dot| Identifier))

(defrule CompilationUnit
  ([PackageDeclaration] {ImportDeclaration} {TypeDeclaration}))

(defrule PackageDeclaration
  ({PackageModifier} |package| Identifier {|dot| Identifier} |semi|))

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
  (|import| |static| TypeName |dot| Identifier |semi|))

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
  ({ClassModifier} |class| Identifier [TypeParameters] [Superclass]
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
  (Identifier [Dims]))

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
  (Identifier [TypeArguments])
  (UnannClassOrInterfaceType |dot| {Annotation} Identifier [TypeArguments]))

(defrule UnannInterfaceType
  (UnannClassType))

(defrule UnannTypeVariable
  (Identifier))

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
  (Identifier |lparen| [FormalParameterList] |rparen| [Dims]))

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
  ({Annotation} UnannType [Identifier |dot|] |this|))

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
  (Identifier))

(defrule ConstructorBody
  (|lparen| [ExplicitConstructorInvocation] [BlockStatements] |rparen|))

(defrule ExplicitConstructorInvocation
  ([TypeArguments] |this| |lparen| [ArgumentList] |rparen| |semi|)
  ([TypeArguments] |super| |lparen| [ArgumentList] |rparen| |semi|)
  (ExpressionName |dot| [TypeArguments] |super| |lparen| [ArgumentList] |rparen| |semi|)
  (Primary |dot| [TypeArguments] |super| |lparen| [ArgumentList] |rparen| |semi|))

(defrule EnumDeclaration
  ({ClassModifier} |enum| Identifier [Superinterfaces] EnumBody))

(defrule EnumBody
  (|lbrace| [EnumConstantList] [|comma|] [EnumBodyDeclarations] |rbrace|))

(defrule EnumConstantList
  (EnumConstant {|comma| EnumConstant}))

(defrule EnumConstant
  ({EnumConstantModifier} Identifier [|lparen| [ArgumentList] |rparen|] [ClassBody]))

(defrule EnumConstantModifier
  (Annotation))

(defrule EnumBodyDeclarations
  (|semi| {ClassBodyDeclaration}))
