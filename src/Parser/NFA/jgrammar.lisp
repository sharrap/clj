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
