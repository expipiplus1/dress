#!/usr/bin/env python3

from modgrammar import *
from modgrammar import Terminal, error_result
from modgrammar.extras import *
import re
from sys import *

#grammar_whitespace = re.compile("\s*")

#
# Grammar classes
#

class END_OF_WORD( Terminal ):
  grammar_collapse = True
  grammar_collapse_skip = True
  grammar_desc = "End of word"

  @classmethod
  def grammar_parse(cls, text, index, sessiondata):
    string = text.string[index:]
    if len( string ) == 0:
        yield (0, cls(""))
    end_of_word_pattern = re.compile( "[^a-zA-Z0-9_]" )
    match = end_of_word_pattern.match( string )
    if match:
        yield (0, cls(""))
    yield error_result(index, cls)

  @classmethod
  def grammar_ebnf_lhs(cls, opts):
    return ("(*end of word*)", ())

  @classmethod
  def grammar_ebnf_rhs(cls, opts):
    return None

#
# C++ Grammar
#

class Identifier( Grammar ):
    #grammar = RE( "[A-Za-z_][A-Za-z0-9_]*" )#WORD( startchars = "a-zA-Z_", restchars="a-zA-Z0-9_" )
    grammar = EXCEPT( RE( "[A-Za-z_][A-Za-z0-9_]*" ), REF( "Keyword" ) )

class AttributeNamespace( Grammar ):
    grammar = Identifier

class AttributeScopedToken( Grammar ):
    grammar = AttributeNamespace, "::", Identifier

class AttributeToken( Grammar ):
    grammar = OR( Identifier,
                  AttributeScopedToken )

class Keyword( Grammar ):
    grammar = OR( "alignas",     #C++0x
                  "alignof",     #C++0x
                  "asm",
                  "auto",
                  "bool",
                  "break",
                  "case",
                  "catch",
                  "char",
                  "char16_t",    #C++0x
                  "char32_t",    #C++0x
                  "class",
                  "const",
                  "constexpr",   #C++0x
                  "const_cast",
                  "continue",
                  "decltype",     #C++0x
                  "default",
                  "delete",
                  "do",
                  "double",
                  "dynamic_cast",
                  "else",
                  "enum",
                  "explicit",
                  "export",     #C++0x - Reserved for future use
                  "extern",
                  "false",
                  "float",
                  "for",
                  "friend",
                  "goto",
                  "if",
                  "inline",
                  "int",
                  "long",
                  "mutable",
                  "namespace",
                  "new",
                  "noexcept",     #C++0x
                  "nullptr",     #C++0x
                  "operator",
                  "private",
                  "protected",
                  "public",
                  "register",
                  "reinterpret_cast",
                  "return",
                  "short",
                  "signed",
                  "sizeof",
                  "static",
                  "static_assert",     #C++0x
                  "static_cast",
                  "struct",
                  "switch",
                  "template",
                  "this",
                  "thread_local",     #C++0x
                  "throw",
                  "true",
                  "try",
                  "typedef",
                  "typeid",
                  "typename",
                  "union",
                  "unsigned",
                  "using",
                  "virtual",
                  "void",
                  "volatile",
                  "wchar_t",
                  "while" ), END_OF_WORD

#class DecimalLiteral( Grammar ):
#    grammar = ( OR ( ( NonZeroDigit ),
#                     ( REF( "DecimalLiteral" ), Digit ) ) )

class DecimalLiteral( Grammar ):
    grammar = RE( "[1-9][0-9]*" )

    def elem_init( self, sessiondata ):
        self.value = int( self.string )

#class OctalLiteral( Grammar ):
#    grammar = ( OR ( ( "0" ),
#                     ( REF( "OctalLiteral" ), OctalDigit ) ) )

class OctalLiteral( Grammar ):
    grammar = RE( "0[0-7]*" )

    def elem_init( self, sessiondata ):
        self.value = int( self.string, base = 8 )

#class HexadecimalLiteral( Grammar ):
#    grammar = ( OR ( ( "0x", HexadecimalDigit ),
#                     ( "0X", HexadecimalDigit ),
#                     ( REF( "HexadecimalLiteral" ), HexadecimalDigit ) ) )

class HexadecimalLiteral( Grammar ):
    grammar = RE ( "0[xX][0-9a-fA-F]" )

    def elem_init( self, sessiondata ):
        self.value = int( self.string[2:], base = 16 )

class UnsignedSuffix( Grammar ):
    grammar = OR( "u",
                  "U" )

class LongSuffix( Grammar ):
    grammar = OR( "l",
                  "L" )

class LongLongSuffix( Grammar ):
    grammar = OR( "ll", #c++0x
                  "LL" ) #c++0x

class IntegerSuffix( Grammar ):
    grammar = OR( ( UnsignedSuffix, OPTIONAL( LongSuffix ) ),
                  ( UnsignedSuffix, OPTIONAL( LongLongSuffix ) ), #c++0x
                  ( LongSuffix, OPTIONAL( UnsignedSuffix ) ),
                  ( LongLongSuffix, OPTIONAL( UnsignedSuffix ) ) ) #c++0x

class IntegerLiteral( Grammar ):
    grammar = OR( ( DecimalLiteral, OPTIONAL( IntegerSuffix ) ),
                  ( OctalLiteral, OPTIONAL( IntegerSuffix ) ),
                  ( HexadecimalLiteral, OPTIONAL( IntegerSuffix ) ) )

class SimpleEscapeSequence( Grammar ):
    grammar = "\\", OR( "'",
                        "\"",
                        "?",
                        "\\",
                        "a",
                        "b",
                        "f",
                        "n",
                        "r",
                        "t",
                        "v" )
    grammar_collapse = True 

class OctalEscapeSequence( Grammar ):
    grammar = RE( "\\\\[0-7]{1,3}" )
    grammar_collapse = True

class HexadecimalEscapeSequence( Grammar ):
    grammar = RE( "\\\\x[0-9a-fA-F]+" )

class EscapeSequence( Grammar ):
    grammar = OR( SimpleEscapeSequence,
                  OctalEscapeSequence,
                  HexadecimalEscapeSequence )

class HexQuad( Grammar ):
    grammar = RE( "[0-9a-fA-F]{4}" )

class UniversalCharacterName( Grammar ):
    grammar = "\\", OR( ( "u", HexQuad ),
                        ( "U", HexQuad, HexQuad ) )

class CChar( Grammar ):
    grammar = OR( EXCEPT( ANY, WORD( "'\\\n" ) ),
                  EscapeSequence,
                  UniversalCharacterName )

class CCharSequence( Grammar ):
    grammar = REPEAT( CChar )
    grammar_collapse = True

class CharacterLiteral( Grammar ):
    grammar = OPTIONAL( OR( "u",
                            "U",
                            "L" ) ), "'", CCharSequence, "'"
    grammar_collapse = True

class DigitSequence( Grammar ):
    grammar = RE( "[0-9]+" )

class FractionalConstant( Grammar ):
    grammar = OR( ( OPTIONAL( DigitSequence ), ".", DigitSequence ),
                  ( DigitSequence, "." ) )

class Sign( Grammar ):
    grammar = OR( "+",
                  "-" )

class ExponentPart( Grammar ):
    grammar = OR( "e",
                  "E" ), OPTIONAL( Sign ), DigitSequence

class FloatingSuffix( Grammar ):
    grammar = OR( "f",
                  "l",
                  "F",
                  "L" )

class FloatingLiteral( Grammar ):
    grammar = OR( ( FractionalConstant, OPTIONAL( ExponentPart ), OPTIONAL( FloatingSuffix ) ),
                  ( DigitSequence, ExponentPart, OPTIONAL( FloatingSuffix ) ) )

class EncodingPrefix( Grammar ):
    grammar = OR( "u8",
                  "u",
                  "U",
                  "L" )

class SCharSequence( Grammar ):
    grammar = REPEAT( OR( RE( "[^\"\\\\\\n]+" ),
                          EscapeSequence,
                          UniversalCharacterName ) )
    grammar_collapse = True
    #def grammar_collapsed_elems( self, sessiondata ):
    #    return []

class StringLiteral( Grammar ):
    grammar = OPTIONAL( EncodingPrefix ), "\"", SCharSequence, "\""

class BooleanLiteral( Grammar ):
    grammar = OR( "false",
                  "true" )

class PointerLiteral( Grammar ):
    grammar = LITERAL( "nullptr" )

class Literal( Grammar ): #Incomplete
    grammar = OR( IntegerLiteral,
                  CharacterLiteral,
                  FloatingLiteral,
                  StringLiteral,
                  BooleanLiteral,
                  PointerLiteral )
                  #UserDefinedLiteral )

class PreprocessingOpOrPunc( Grammar ):
    grammar = OR( "{",
                  "}",
                  "[",
                  "]",
                  "#",
                  "##",
                  "(",
                  ")",
                  "<:",
                  ":>",
                  "<%",
                  "%>",
                  "%:",
                  "%:%:",
                  ";",
                  ":",
                  "...",
                  "new",
                  "delete",
                  "?",
                  "::",
                  ".",
                  ".*",
                  "+",
                  "-",
                  "*",
                  "/",
                  "%",
                  "^",
                  "&",
                  "|",
                  "~",
                  "!",
                  "=",
                  "<",
                  ">",
                  "+=",
                  "-=",
                  "*=",
                  "/=",
                  "%=",
                  "^=",
                  "&=",
                  "|=",
                  "<<",
                  ">>",
                  "<<=",
                  ">>=",
                  "==",
                  "!=",
                  "<=",
                  ">=",
                  "&&",
                  "||",
                  "++",
                  "--",
                  ",",
                  "->*",
                  "->",
                  "and",
                  "and_eq",
                  "bitand",
                  "bitor",
                  "compl",
                  "not",
                  "not_eq",
                  "or",
                  "xor",
                  "xor_eq" )

class Token( Grammar ):
    grammar = OR( Identifier,
                  Keyword,
                  Literal,
                  PreprocessingOpOrPunc )

class BalancedToken( Grammar ):
    grammar = OR( ( "(", REF( "BalancedTokenSeq" ), ")" ),
                  ( "[", REF( "BalancedTokenSeq" ), "]" ),
                  ( "{", REF( "BalancedTokenSeq" ), "}" ),
                  EXCEPT( Token, WORD( "()[]{}" ) ) )
    grammar_collapse = True

class BalancedTokenSeq( Grammar ):
    grammar = REPEAT( BalancedToken, min = 0 )
    grammar_collapse = True

class BalancedToken_Template( Grammar ):
    grammar = OR( ( "(", BalancedTokenSeq, ")" ),
                  ( "[", BalancedTokenSeq, "]" ),
                  ( "{", BalancedTokenSeq, "}" ),
                  ( "<", REF( "BalancedTokenSeq_Template" ), ">" ),
                  EXCEPT( Token, WORD( "()[]{}<>" ) ) )
    grammar_collapse = True

class BalancedTokenSeq_Template( Grammar ):
    grammar = REPEAT( BalancedToken_Template, min = 0 )
    grammar_collapse = True

class AttributeArgumentClause( Grammar ):
    grammar = "(", BalancedTokenSeq, ")"

class Attribute( Grammar ):
    grammar = AttributeToken, OPTIONAL( AttributeArgumentClause )

class AttributeList( Grammar ):
    grammar = LIST_OF( ( Attribute, OPTIONAL( "..." ) ), sep = "," ), OPTIONAL( "," )

class TemplateName( Grammar ):
    grammar = Identifier

class TemplateArgument( Grammar ): #Incomplete
    grammar = REPEAT( EXCEPT( BalancedToken_Template, RE( "," ) ), collapse = True )
    #grammar = OR( ConstantExpression,
    #              REF( "TypeId" ),
    #              IdExpression )

class TemplateArgumentList( Grammar ):
    grammar = LIST_OF( ( TemplateArgument, OPTIONAL( "..." ) ), sep = "," )

class SimpleTemplateId( Grammar ):
    grammar = TemplateName, "<", OPTIONAL( TemplateArgumentList ), ">"

class ClassName( Grammar ):
    grammar = OR( Identifier,
                  SimpleTemplateId )

class EnumName( Grammar ):
    grammar = Identifier

class TypedefName( Grammar ):
    grammar = Identifier

class TypeName( Grammar ):
    grammar = OR( Identifier, #ClassName,
                  #EnumName,
                  #TypedefName )
                  SimpleTemplateId )

class OriginalNamespaceName( Grammar ):
    grammar = Identifier

class NamespaceAlias( Grammar ):
    grammar = Identifier

class NamespaceName( Grammar ):
    grammar = OR( OriginalNamespaceName,
                  NamespaceAlias )

class DecltypeSpecifier( Grammar ):
    grammar = "decltype", "(", BalancedTokenSeq, ")"

class NestedNameSpecifier( Grammar ):
    grammar = OPTIONAL( ( OR( TypeName,
                              NamespaceName,
                              DecltypeSpecifier ), "::" ) ), \
              REPEAT( ( OR( Identifier,
                            ( OPTIONAL( "template" ), SimpleTemplateId ) ), "::" ), min = 0 )

class SimpleTypeSpecifier( Grammar ):
    grammar = OR( ( OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), TypeName ),
                  ( OPTIONAL( "::" ), NestedNameSpecifier, "template", SimpleTemplateId ), 
                  "char",
                  "char16_t", #C++0x
                  "char32_t", #C++0x
                  "wchar_t",
                  "bool",
                  "short",
                  "int",
                  "long",
                  "signed",
                  "unsigned",
                  "float",
                  "double",
                  "void",
                  "auto",
                  DecltypeSpecifier )

class ClassKey( Grammar ):
    grammar = OR( "class",
                  "struct",
                  "union" )

class ElaboratedTypeSpecifier( Grammar ):
    grammar = OR( ( ClassKey, OPTIONAL( REF( "AttributeSpecifierSeq" ) ), "::", OPTIONAL( NestedNameSpecifier ), Identifier ),
                  ( ClassKey, OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), OPTIONAL( "template" ), SimpleTemplateId ),
                  ( "enum", OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), Identifier ) )

class TypenameSpecifier( Grammar ):
    grammar = "typename", OPTIONAL( "::" ), NestedNameSpecifier, OR( Identifier,
                                                                     ( OPTIONAL( "template" ), SimpleTemplateId ) )

class CvQualifier( Grammar ):
    grammar = OR( "const",
                  "volatile" )

class CvQualifierSeq( Grammar ):
    grammar = REPEAT( CvQualifier )

class TrailingTypeSpecifier( Grammar ):
    grammar = OR( SimpleTypeSpecifier,
                  ElaboratedTypeSpecifier,
                  TypenameSpecifier,
                  CvQualifier )

class TrailingTypeSpecifierSeq( Grammar ):
    grammar = REPEAT( TrailingTypeSpecifier ), OPTIONAL( REF( "AttributeSpecifierSeq" ) )

class ClassHeadName( Grammar ):
    grammar = OPTIONAL( NestedNameSpecifier ), ClassName

class ClassVirtSpecifier( Grammar ):
    grammar = OR( "final", 
                  "explicit" )

class ClassVirtSpecifierSeq( Grammar ):
    grammar = REPEAT( ClassVirtSpecifier )

class AccessSpecifier( Grammar ):
    grammar = OR( "private",
                  "protected",
                  "public" )

class ClassOrDecltype( Grammar ):   
    grammar = OR( ( OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), ClassName ),
                  DecltypeSpecifier )

class BaseTypeSpecifier( Grammar ):
    grammar = ClassOrDecltype

class BaseSpecifier( Grammar ):
    grammar = OPTIONAL( REF( "AttributeSpecifierSeq" ) ), OPTIONAL( OR( ( "virtual", OPTIONAL( AccessSpecifier ) ),
                                                               ( AccessSpecifier, OPTIONAL( "virtual" ) ) ) ), BaseTypeSpecifier

class BaseSpecifierList( Grammar ):
    grammar = LIST_OF( ( BaseSpecifier, OPTIONAL( "..." ) ), sep = "," )

class BaseClause( Grammar ):
    grammar = ":", BaseSpecifierList

class ClassHead( Grammar ):
    grammar = ClassKey, OPTIONAL( REF( "AttributeSpecifierSeq" ) ), OR( ( ClassHeadName, OPTIONAL( ClassVirtSpecifierSeq ), OPTIONAL( BaseClause ) ),
                                                                        OPTIONAL( BaseClause ) )

class VirtSpecifier( Grammar ):
    grammar = OR( LITERAL( "override" ),
                  LITERAL( "final" ),
                  LITERAL( "new" ) )

class VirtSpecifierSeq( Grammar ):
    grammar = REPEAT( VirtSpecifier )

class PureSpecifier( Grammar ):
    grammar = LITERAL( "=" ), LITERAL( "0" )

class InitializerList( Grammar ):
    grammar = LIST_OF( ( REF( "InitializerClause" ), OPTIONAL( "..." ) ), sep = "," )

class BracedInitList( Grammar ):
    grammar = "{", OPTIONAL( InitializerList, OPTIONAL( "," ) ), "}"

class InitializerClause( Grammar ):
    grammar = OR( BracedInitList,
                  REPEAT( EXCEPT( BalancedToken_Template, RE( ",|;" ) ), collapse = True ) )

class BraceOrEqualInitializer( Grammar ):
    grammar = OR( ( "=", InitializerClause ),
                  BracedInitList )

class MemberDeclarator( Grammar ): #incomplete
    grammar = OR( ( REF( "Declarator" ), OPTIONAL( VirtSpecifierSeq ), OPTIONAL( OR( PureSpecifier,
                                                                                     BraceOrEqualInitializer ) ) ),
                  ( OPTIONAL( Identifier ), OPTIONAL( REF( "AttributeSpecifierSeq" ) ), OPTIONAL( VirtSpecifierSeq ), ":", REPEAT( EXCEPT( BalancedToken_Template, WORD( ",;" ) ), collapse = True ) ) ) 

class MemberDeclaratorList( Grammar ):
    grammar = LIST_OF( MemberDeclarator, sep = ",", collapse = True )

class OverloadableOperator( Grammar ):
    grammar = OR( "new",
                  "delete",
                  ( L("new"), L("["), L("]") ),
                  ( L("delete"), L("["), L("]") ),
                  "+",
                  "-",
                  "*",
                  "/",
                  "%",
                  "^",
                  "&",
                  "|",
                  "~",
                  "!",
                  "=",
                  "<",
                  ">",
                  "+=",
                  "-=",
                  "*=",
                  "/=",
                  "%=",
                  "^=",
                  "&=",
                  "|=",
                  "<<",
                  ">>",
                  ">>=",
                  "<<=",
                  "==",
                  "!=",
                  "<=",
                  ">=",
                  "&&",
                  "||",
                  "++",
                  "--",
                  ",",
                  "->*",
                  "->",
                  ( L("("), L(")") ),
                  ( L("["), L("]") ) )

class OperatorFunctionId( Grammar ):
    grammar = "operator", OverloadableOperator, OPTIONAL( "<", OPTIONAL( TemplateArgumentList ), ">" )

class ConversionDeclarator( Grammar ):
    grammar = REPEAT( REF( "PtrOperator" ) )

class ConversionTypeId( Grammar ):
    grammar = REF( "TypeSpecifierSeq" ), OPTIONAL( ConversionDeclarator )

class ConversionFunctionId( Grammar ):
    grammar = "operator", ConversionTypeId

class LiteralOperatorId( Grammar ):
    grammar = "operator", "\"", "\"", Identifier

class TemplateId( Grammar ):
    grammar = OR( REF( "SimpleTemplateId" ),
                  ( OR( OperatorFunctionId,
                        LiteralOperatorId ), "<", OPTIONAL( TemplateArgumentList ), ">" ) )

class UnqualifiedId( Grammar ):
    grammar = OR( Identifier,
                  OperatorFunctionId,
                  ConversionFunctionId,
                  LiteralOperatorId,
                  ( "~", ClassName ),
                  ( "~", DecltypeSpecifier ),
                  TemplateId )

class UsingDeclaration( Grammar ):
    grammar = "using", OR( ( OPTIONAL( "typename" ), OPTIONAL( "::" ), NestedNameSpecifier, UnqualifiedId, ";" ),
                           ( "::", UnqualifiedId, ";" ) )

class StaticAssertDeclaration( Grammar ):
    grammar = "static_assert", "(", REPEAT( EXCEPT( BalancedToken_Template, WORD( ",;" ) ), collapse = True ), ",", StringLiteral, ")", ";"

class QualifiedId( Grammar ):
    grammar = OR( ( OPTIONAL( "::" ), NestedNameSpecifier, OPTIONAL( "template"  ), UnqualifiedId ),
                  ( "::", OR( Identifier,
                              OperatorFunctionId,
                              LiteralOperatorId,
                              TemplateId ) ) )

class IdExpression( Grammar ):
    grammar = OR( UnqualifiedId,
                  QualifiedId )

class TypeParameter( Grammar ):
    grammar = OR( ( OR( "class", 
                        "typename" ), OR( ( OPTIONAL( "..." ), OPTIONAL( Identifier ) ),
                                          ( OPTIONAL( Identifier ), "=", REF( "TypeId" ) ) ) ),
                  ( "template", "<", REF( "TemplateParameterList" ), ">", "class", OR( ( OPTIONAL( "..." ), OPTIONAL( Identifier ) ),
                                                                              ( OPTIONAL( Identifier ), "=", IdExpression ) ) ) )

class ParameterDeclaration( Grammar ):
    grammar = OPTIONAL( REF( "AttributeSpecifierSeq" ) ), REF( "DeclSpecifierSeq" ), OR( REF( "Declarator" ),
                                                                                OPTIONAL( REF( "AbstractDeclarator" ) ) ), OPTIONAL( "=", InitializerClause )

class TemplateParameter( Grammar ):
    grammar = OR( TypeParameter,
                  ParameterDeclaration )

class TemplateParameterList( Grammar ):
    grammar = LIST_OF( TemplateParameter, sep = "," )

class TemplateDeclaration( Grammar ):
    grammar = "template", "<", TemplateParameterList, ">", REF( "Declaration" )

class AliasDeclaration( Grammar ):
    grammar = "using", Identifier, "=", REF( "TypeId" ), ";"

class MemberDeclaration( Grammar ):
    grammar = OR( ( OPTIONAL( REF( "AttributeSpecifierSeq" ) ), OPTIONAL( REF( "DeclSpecifierSeq" ) ), OPTIONAL( MemberDeclaratorList ), ";" ),
                  ( REF( "FunctionDefinition" ), OPTIONAL( ";" ) ),
                  UsingDeclaration,
                  StaticAssertDeclaration,
                  TemplateDeclaration,
                  AliasDeclaration )

class MemberSpecification( Grammar ):
    grammar = OR( MemberDeclaration,
                  ( AccessSpecifier, ":" ) )

class MemberSpecificationSeq( Grammar ):
    grammar = REPEAT( MemberSpecification, collapse = True )
    grammar_collapse = True

class ClassSpecifier( Grammar ):
    grammar = ClassHead, "{", OPTIONAL( MemberSpecificationSeq ), "}"

class EnumKey( Grammar ):
    grammar = "enum", OPTIONAL( OR( "class",
                                    "struct" ) )

class EnumBase( Grammar ):
    grammar = ":", REF( "TypeSpecifierSeq" )

class EnumHead( Grammar ):
    grammar = EnumKey, OPTIONAL( REF( "AttributeSpecifierSeq" ) ), OR( OPTIONAL( Identifier ),
                                                                       ( NestedNameSpecifier, Identifier ) ), OPTIONAL( EnumBase )

class Enumerator( Grammar ):
    grammar = Identifier

class EnumeratorDefinition( Grammar ):
    grammar = Enumerator, OPTIONAL( "=", REPEAT( EXCEPT( BalancedToken_Template, WORD( ",;}" ) ), collapse = True ) )

class EnumeratorList( Grammar ):
    grammar = LIST_OF( EnumeratorDefinition, sep = "," )

class EnumSpecifier( Grammar ):
    grammar = EnumHead, "{", OPTIONAL( EnumeratorList, OPTIONAL( "," ) ), "}"

class TypeSpecifier( Grammar ):
    grammar = OR( TrailingTypeSpecifier,
                  ClassSpecifier,
                  EnumSpecifier )

class TypeSpecifierSeq( Grammar ):
    grammar = REPEAT( TypeSpecifier ), OPTIONAL( REF( "AttributeSpecifierSeq" ) )

class TypeId( Grammar ): 
    grammar = TypeSpecifierSeq, OPTIONAL( REF( "AbstractDeclarator" ) )

class AlignmentSpecifier( Grammar ): #Incomplete
    grammar = "alignas", "(", OR( TypeId,
                                  REPEAT( EXCEPT( BalancedToken_Template, RE( "\\.{3}|\\)" ) ), collapse = True ) ), OPTIONAL( "..." ), ")"

class AttributeSpecifier( Grammar ):
    grammar = OR( ( "[", "[", AttributeList, "]", "]" ),
                  AlignmentSpecifier )

class AttributeSpecifierSeq( Grammar ):
    grammar = REPEAT( AttributeSpecifier, collapse = True, greedy = False )

class StorageClassSpecifier( Grammar ):
    grammar = OR( "auto",
                  "register",
                  "static",
                  "thread_local",
                  "extern",
                  "mutable" )

class FunctionSpecifier( Grammar ):
    grammar = OR( "inline",
                  "virtual",
                  "explicit" )

class DeclSpecifier( Grammar ):
    grammar = OR( StorageClassSpecifier,
                  TypeSpecifier,
                  FunctionSpecifier,
                  "friend",
                  "typedef",
                  "constexpr" )

class DeclSpecifierSeq( Grammar ):
    grammar = REPEAT( DeclSpecifier, collapse = True, greedy = False ), OPTIONAL( AttributeSpecifierSeq )

class DeclaratorId( Grammar ):
    grammar = OR( ( OPTIONAL( "..." ), IdExpression ),
                  ( OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), ClassName ) )

class ParameterDeclarationList( Grammar ):
    grammar = LIST_OF( ParameterDeclaration, sep = ",", collapse = True )

class ParameterDeclarationClause( Grammar ):
    grammar = OR( ( OPTIONAL( ParameterDeclarationList ), OPTIONAL( "..." ) ),
                  ( ParameterDeclarationList, ",", "..." ) )

class RefQualifier( Grammar ):
    grammar = OR( "&",
                  "&&" )

class TypeIdList( Grammar ):
    grammar = LIST_OF( (TypeId, OPTIONAL( "..." ) ), sep = "," )

class DynamicExceptionSpecification( Grammar ):
    grammar = "throw", "(", OPTIONAL( TypeIdList ), ")"

class NoexceptSpecification( Grammar ):
    grammar = "noexcept", OPTIONAL( "(", REPEAT( EXCEPT( BalancedToken_Template, WORD( ")" ) ), collapse = True ), ")" )

class ExceptionSpecification( Grammar ):
    grammar = OR( DynamicExceptionSpecification,
                  NoexceptSpecification )

class ParametersAndQualifiers( Grammar ):
    grammar = "(", ParameterDeclarationClause, ")", OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( CvQualifierSeq ), OPTIONAL( RefQualifier ), OPTIONAL( ExceptionSpecification )

class NoptrDeclarator( Grammar ):
    grammar = OR( ( DeclaratorId, OPTIONAL( AttributeSpecifierSeq ) ),
                  ( "(", REF( "PtrDeclarator" ), ")" ) ), REPEAT( OR( ParametersAndQualifiers,
                                                                      ( "[", REPEAT( EXCEPT( BalancedToken_Template, WORD( "]" ) ), min = 0 ), "]", OPTIONAL( AttributeSpecifierSeq ) ) ), min = 0 )

class PtrOperator( Grammar ):
    grammar = OR( ( "*", OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( CvQualifierSeq ) ),
                  ( OR( "&",
                        "&&" ), OPTIONAL( AttributeSpecifierSeq ) ),
                  ( OPTIONAL( "::" ), NestedNameSpecifier, "*", OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( CvQualifierSeq ) ) )

class PtrDeclarator( Grammar ):
    grammar = OR( NoptrDeclarator,
                  ( PtrOperator, REF( "PtrDeclarator" ) ) )

class NoptrAbstractDeclarator( Grammar ):
    grammar = "(", REF( "PtrAbstractDeclarator" ), ")", REPEAT( OR( ParametersAndQualifiers,
                                                                    ( "[", REPEAT( EXCEPT( BalancedToken_Template, WORD( "]" ) ), collapse = True ), "]", OPTIONAL( AttributeSpecifierSeq ) ) ) )

class PtrAbstractDeclarator( Grammar ):
    grammar = OR( NoptrAbstractDeclarator,
                  ( PtrOperator, OPTIONAL( REF( "PtrAbstractDeclarator" ) ) ) )

class AbstractDeclarator( Grammar ):
    grammar = OR( PtrAbstractDeclarator,
                  ( OPTIONAL( NoptrAbstractDeclarator ), ParametersAndQualifiers, REF( "TrailingReturnType" ) ),
                  "..." )

class TrailingReturnType( Grammar ):
    grammar = "->", TrailingTypeSpecifierSeq, OPTIONAL( AbstractDeclarator )

class Declarator( Grammar ):
    grammar = OR( PtrDeclarator,
                  ( NoptrDeclarator, ParametersAndQualifiers, TrailingReturnType ) )

class CompoundStatement( Grammar ): #Incomplete
    grammar = "{", OPTIONAL( REPEAT( OR( RE( "[^{}\\\"]+" ),
                                         REF( "CompoundStatement" ),
                                         QuotedString ) ) ), "}"

class MemInitializerId( Grammar ):
    grammar = OR( ClassOrDecltype,
                  Identifier )

class MemInitializer( Grammar ):
    grammar = MemInitializerId, OR( ( "(", REPEAT( EXCEPT( BalancedToken_Template, WORD( ")" ) ), min = 0 ), ")" ),
                                    BracedInitList )

class MemInitializerList( Grammar ):
    grammar = LIST_OF( ( MemInitializer, OPTIONAL( "..." ) ), sep = "," )

class CtorInitializer( Grammar ):
    grammar = ":", MemInitializerList

class ExceptionDeclaration( Grammar ):
    grammar = OR( ( OPTIONAL( AttributeSpecifierSeq ), TypeSpecifierSeq, OR( Declarator,
                                                                             OPTIONAL( AbstractDeclarator ) ) ),
                  "..." )

class Handler( Grammar ):
    grammar = "catch", "(", ExceptionDeclaration, ")", CompoundStatement

class HandlerSeq( Grammar ):
    grammar = REPEAT( Handler )

class FunctionTryBlock( Grammar ):
    grammar = "try", OPTIONAL( CtorInitializer ), CompoundStatement, HandlerSeq

class FunctionBody( Grammar ): 
    grammar = OR( ( OPTIONAL( CtorInitializer ), CompoundStatement ),
                  FunctionTryBlock )

class FunctionDefinition( Grammar ):
    grammar = OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( DeclSpecifierSeq ), Declarator, OR( FunctionBody,
                                                                                               ( "=", OR( "default",
                                                                                                          "delete" ), ";" ) )

class Initializer( Grammar ):
    grammar = OR( BraceOrEqualInitializer,
                  ( "(", REPEAT( EXCEPT( BalancedToken_Template, WORD( ")" ) ) ), ")" ) )

class InitDeclarator( Grammar ):
    grammar = Declarator, OPTIONAL( Initializer )

class InitDeclaratorList( Grammar ):
    grammar = LIST_OF( InitDeclarator, sep = "," )

class SimpleDeclaration( Grammar ):
    grammar = OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( DeclSpecifierSeq ), OPTIONAL( InitDeclaratorList ), ";"

class AsmDefinition( Grammar ):
    grammar = "asm", "(", StringLiteral, ")"

class QualifiedNamespaceSpecifier( Grammar ):
    grammar = OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), NamespaceName

class NamespaceAliasDefinition( Grammar ):
    grammar = "namespace", Identifier, "=", QualifiedNamespaceSpecifier, ";"

class UsingDirective( Grammar ):
    grammar = OPTIONAL( AttributeSpecifierSeq ), "using", "namespace", OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), NamespaceName, ";"

class OpaqueEnumDeclaration( Grammar ):
    grammar = EnumKey, OPTIONAL( AttributeSpecifierSeq ), Identifier, OPTIONAL( EnumBase ), ";"

class BlockDeclaration( Grammar ):
    grammar = OR( SimpleDeclaration,
                  AsmDefinition,
                  NamespaceAliasDefinition,
                  UsingDeclaration,
                  UsingDirective,
                  StaticAssertDeclaration,
                  AliasDeclaration,
                  OpaqueEnumDeclaration )

class ExplicitInstantiation( Grammar ):
    grammar = OPTIONAL( "extern" ), "template", REF( "Declaration" )

class ExplicitSpecialization( Grammar ):
    grammar = "template", "<", ">", REF( "Declaration" )

class LinkageSpecification( Grammar ):
    grammar = "extern", StringLiteral, OR( ( "{", OPTIONAL( REF( "DeclarationSeq" ) ), "}" ),
                                           REF( "Declaration" ) ) 

class NamespaceBody( Grammar ):
    grammar = OPTIONAL( REF( "DeclarationSeq" ) )

class NamedNamespaceDefinition( Grammar ): #Modified
    grammar = OPTIONAL( "inline" ), "namespace", Identifier, "{", NamespaceBody, "}"

class UnnamedNamespaceDefinition( Grammar ):
    grammar = OPTIONAL( "inline" ), "namespace", "{", NamespaceBody, "}" 

class NamespaceDefinition( Grammar ):
    grammar = OR( NamedNamespaceDefinition,
                  UnnamedNamespaceDefinition )

class EmptyDeclaration( Grammar ):
    grammar = ";"

class AttributeDeclaration( Grammar ):
    grammar = AttributeSpecifierSeq, ";"

class Declaration( Grammar ):
    grammar = OR( BlockDeclaration,
                  FunctionDefinition,
                  TemplateDeclaration,
                  ExplicitInstantiation,
                  ExplicitSpecialization,
                  LinkageSpecification,
                  NamespaceDefinition,
                  EmptyDeclaration,
                  AttributeDeclaration )

class DeclarationSeq( Grammar ):
    grammar = REPEAT( Declaration, collapse = True )
    grammar_collapse = True

class TranslationUnit( Grammar ):
    grammar = DeclarationSeq, EOF

    def elem_init( self, sessiondata ):
        self.start = 0
        self.end = len( self.string )
    
#
# Functions
#

def PrintIndented( string, indentation ):
    for i in range( 0, indentation ):
        stdout.write( " " )
    for c in string:
        stdout.write( c )
        if c == "\n":
            for i in range( 0, indentation ):
                stdout.write( " " )
    stdout.write( "\n" )

def PrintElements( element, indentation = 0 ):
    PrintIndented( element.__repr__(), indentation )
    if not element:
        return
    #PrintIndented( "POSITION: " + str( element.start ) , indentation )
    for e in element.elements:
        PrintElements( e, indentation + 4 )

def RemoveNoneElements( elements ):
    i = 0
    elements = list( elements )
    while i < len( elements ):
        element = elements[i]
        if element:
            element.elements = RemoveNoneElements( element.elements )
        else:
            del elements[i]
        i += 1
    elements = tuple( elements )
    return elements

def PrintElementStrings( element, string ):
    if not element:
        return
    if not element.elements:
        stdout.write( string[element.start:element.end] )
    else:
        first_non_none_subelement = None
        last_non_none_subelement = None

        for subelement in element.elements:
            if subelement:
                first_non_none_subelement = subelement
                break

        for subelement in reversed( element.elements ):
            if subelement:
                last_non_none_subelement = subelement
                break

        if not first_non_none_subelement:
            stdout.write( string[element.start:element.end] )
            return

        stdout.write( string[element.start:first_non_none_subelement.start] )
        for i, subelement in enumerate( element.elements ):
            if not subelement:
                continue
            PrintElementStrings( subelement, string )
            if i != len( element.elements ) - 1:
                next_non_none_subelement = None
                for next_subelement in element.elements[i+1:]:
                    if next_subelement:
                        next_non_none_subelement = next_subelement
                        break
                if next_non_none_subelement:
                    stdout.write( string[ subelement.end: next_non_none_subelement.start ] )
        stdout.write( string[last_non_none_subelement.end:element.end] )

def CalculateElementsRanges( elements, string, offset = 0 ):
    for element in elements:
        if not element:
            continue
        element.start = string.index( element.string, offset )
        element.end   = element.start + len( element.string )
        offset = element.end
        CalculateElementsRanges( element.elements, string, element.start )

def RemoveComments( string ):
    string_literal_pattern = re.compile( "\"(?:\\\\\"|[^\"])*\"" )
    block_comment_pattern = re.compile( "/\*.*?\*/", re.MULTILINE | re.DOTALL )
    line_comment_pattern = re.compile( "//.*?$", re.MULTILINE )
    preprocessor_pattern = re.compile( "\\#(.*?($|\\\\.*?^))*$", re.MULTILINE | re.DOTALL )

    ret = ""

    i = 0
    while i < len( string ):
        match = string_literal_pattern.match( string[i:] )
        if match:
            ret += match.group()
            i += len( match.group() )
            continue

        match = block_comment_pattern.match( string[i:] )
        if not match:
            match = line_comment_pattern.match( string[i:] )
        if not match:
            match = preprocessor_pattern.match( string[i:] )
            
        if match:
            print ( "START" )
            print( match.group() )
            print ( "END" )
            for j in match.group():
                ret += " "
            i += len( match.group() )
            continue 

        #
        # Nothing has matched
        #
        ret += string[i]
        i += 1

    return ret

def main():
    #parser = Identifier.parser()
    #stdout.writelines( generate_ebnf(Identifier) )
    #string = "float"
    #result = parser.parse_string( string, reset = True, eof = True )
    #PrintElements( result )
    #exit()
    
    #TranslationUnit.grammar_resolve_refs( )
   
    string = stdin.read()

    string = RemoveComments( string )
    print( string )
    exit()

    parser = TranslationUnit.parser()
    result = parser.parse_string( string, reset = True, eof = True )
    if not result:
        print( "Failed to parse" )
        return
    CalculateElementsRanges( result, string )
    PrintElementStrings( result, string )
    print()
    PrintElements( result )

if __name__ == "__main__":
    main()

