#!/usr/bin/env python3

from modgrammar import *
from modgrammar import Terminal, error_result, util, GrammarClass
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
    grammar_whitespace = False
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

def BALANCED_TOKENS( opening_char, closing_char, **kwargs ):
    cdict = util.make_classdict( BalancedGrammar, (), kwargs, opening_char = opening_char, closing_char = closing_char )
    return GrammarClass( "<BalancedGrammar>", ( BalancedGrammar, ), cdict )

class BalancedGrammar (Terminal):
    grammar_desc = "Balanced Braces"
    opening_char = None
    closing_char = None

    @classmethod
    def grammar_parse(cls, text, index, sessiondata):
        i = index
        depth = 0
        string = text.string
        while i < len( string ):
            c = string[i]
            if c == cls.opening_char:
                depth += 1
            elif c == cls.closing_char:
                depth -= 1

            elif c == "\"":
                i += 1
                while i < len( string ):
                    if string[i] == "\"" and string[i-1] != "\\":
                        break
                    i += 1
            elif c == "'":
                i += 1
                while i < len( string ):
                    if string[i] == "'" and string[i-1] != "\\":
                        break
                    i += 1
            if depth < 0:
                yield( i - index, cls( string[index:i] ) )
                break
            i += 1
        yield error_result( index, cls )
                           
    @classmethod
    def grammar_ebnf_lhs(cls, opts):
      return (util.ebnf_specialseq(cls, opts), ())

    @classmethod
    def grammar_ebnf_rhs(cls, opts):
      return None

def BALANCED_UNTIL_TOKENS( ending_chars, use_templates = False, **kwargs ):
    bracket_pairs = [ ( "{", "}" ),
                      ( "(", ")" ),
                      ( "[", "]" ) ]

    independent_bracket_pair = ( "<", ">" ) if use_templates else ()
    
    cdict = util.make_classdict( BalancedUntilGrammar, (), kwargs, ending_chars = ending_chars, bracket_pairs = bracket_pairs, independent_bracket_pair = independent_bracket_pair )
    return GrammarClass( "<BalancedUntilGrammar>", ( BalancedUntilGrammar, ), cdict )

class BalancedUntilGrammar (Terminal):
    grammar_desc = "Balanced Until"
    ending_chars = None
    bracket_pairs = None
    independent_bracket_pair = None

    @classmethod
    def read_until(cls, string, index, stop_chars ):
        i = index
        while i < len( string ):
            c = string[i]
            if c in stop_chars:
                return i
            if c == "\"":
                i += 1
                while i < len( string ):
                    if string[i] == "\"" and string[i-1] != "\\":
                        break
                    i += 1
            elif c == "'":
                i += 1
                while i < len( string ):
                    if string[i] == "'" and string[i-1] != "\\":
                        break
                    i += 1
            i += 1
        return None

    @classmethod
    def grammar_parse(cls, text, index, sessiondata):
        i = index
        ind_depth = 0
        string = text.string
        while i < len( string ):
            c = string[i]
            if c in cls.ending_chars and ind_depth <= 0:
                yield( i - index, cls( string[index:i] ) )
                break
            if len( cls.independent_bracket_pair ) > 0:
                if c == cls.independent_bracket_pair[0]:
                    ind_depth += 1
                elif c == cls.independent_bracket_pair[1]:
                    ind_depth = max( 0, ind_depth - 1 )
            for bracket_pair in cls.bracket_pairs:
                if c == bracket_pair[0]:
                    depth = 1
                    while depth > 0:
                        i += 1
                        i = cls.read_until( string, i, ( bracket_pair[0], bracket_pair[1] ) )
                        if not i:
                            yield error_result( index, cls )
                        if string[i] == bracket_pair[0]:
                            depth += 1
                        else:
                            depth -= 1
                    break
            i += 1
        yield error_result( index, cls )
                           
    @classmethod
    def grammar_ebnf_lhs(cls, opts):
      return (util.ebnf_specialseq(cls, opts), ())

    @classmethod
    def grammar_ebnf_rhs(cls, opts):
      return None

#
# C++ Grammar
#
    
class Identifier( Grammar ):
    grammar = EXCEPT( RE( "[A-Za-z_][A-Za-z0-9_]*" ), REF( "Keyword" ) )
    grammar_whitespace = False

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
    grammar_whitespace = False

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
    grammar = OR( RE( "[^'\\\\\\n]" ),
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
    grammar = OPTIONAL( EncodingPrefix ), "\"", OPTIONAL( SCharSequence ), "\""

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
    grammar = RE( "[^\"]|\"(?:[^\\\\]|\\\\\")*\"" ) 

class AlignmentSpecifier( Grammar ): #Incomplete
    grammar = "alignas", "(", BALANCED_TOKENS( "(", ")" ), ")"

class AttributeSpecifier( Grammar ): #Incomplete
    grammar = OR( ( "[", "[", BALANCED_TOKENS( "[", "]" ), "]", "]" ),
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
                  
class EnumName( Grammar ):
    grammar = Identifier

class TypedefName( Grammar ):
    grammar = Identifier

class NamespaceName( Grammar ):
    grammar = Identifier

class DecltypeSpecifier( Grammar ):
    #grammar = "decltype", "(", REPEAT( Token, greedy = False, collapse = True ), ")"
    grammar = "decltype", "(", BALANCED_TOKENS( "(", ")" ), ")"

class TemplateArgumentList( Grammar ):
    #grammar = REPEAT( Token, greedy = False, collapse = True )
    grammar = BALANCED_UNTIL_TOKENS( ">", True )

class SimpleTemplateId( Grammar ):
    grammar = Identifier, "<", OPTIONAL( TemplateArgumentList ), ">"

class SimpleTemplateId_Suffix( Grammar ):
    grammar = "<", OPTIONAL( TemplateArgumentList ), ">"

class TypeName( Grammar ):
    grammar = Identifier, OPTIONAL( SimpleTemplateId_Suffix )

class NestedNameSpecifier( Grammar ):
    grammar = OR( TypeName,
                  DecltypeSpecifier ), "::", \
              REPEAT( ( Identifier, OPTIONAL( OPTIONAL( "template" ), SimpleTemplateId_Suffix ), "::" ), min = 0, collapse = True )

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
    grammar = OR( ( ClassKey, OPTIONAL( REF( "AttributeSpecifierSeq" ) ), OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), Identifier ),
                  ( ClassKey, OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), OPTIONAL( "template" ), SimpleTemplateId ),
                  ( "enum", OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), Identifier ) )
                  
class TypenameSpecifier( Grammar ):
    grammar = "typename", OPTIONAL( "::" ), NestedNameSpecifier, Identifier, OPTIONAL( ( OPTIONAL( "template" ), SimpleTemplateId_Suffix ) )

class CvQualifier( Grammar ):
    grammar = OR( "const",
                  "volatile" )
                  
class TrailingTypeSpecifier( Grammar ):
    grammar = OR( SimpleTypeSpecifier,
                  ElaboratedTypeSpecifier,
                  TypenameSpecifier,
                  CvQualifier )

class ClassName( Grammar ):
    grammar = Identifier, OPTIONAL( SimpleTemplateId_Suffix )

class ClassHeadName( Grammar ):
    grammar = OPTIONAL( NestedNameSpecifier ), ClassName

class ClassVirtSpecifier( Grammar ):
    grammar = OR( "final", 
                  "explicit" )

class AccessSpecifier( Grammar ):
    grammar = OR( "private",
                  "protected",
                  "public" )

class ClassOrDecltype( Grammar ):   
    grammar = OR( ( OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), ClassName ),
                  DecltypeSpecifier )

class BaseSpecifier( Grammar ):
    grammar = OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( OR( ( "virtual", OPTIONAL( AccessSpecifier ) ),
                                                               ( AccessSpecifier, OPTIONAL( "virtual" ) ) ) ), ClassOrDecltype

class BaseClause( Grammar ):
    grammar = ":", LIST_OF( ( BaseSpecifier, OPTIONAL( "..." ) ), sep = "," )
    
class ClassHead( Grammar ):
    grammar = ClassKey, OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( ClassHeadName, REPEAT( ClassVirtSpecifier, min = 0 ) ), OPTIONAL( BaseClause )

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
    grammar = "operator", OverloadableOperator, OPTIONAL( "<", TemplateArgumentList, ">" )


class TypeSpecifierSeq( Grammar ):
    grammar = REPEAT( REF( "TypeSpecifier" ) ), OPTIONAL( AttributeSpecifierSeq )


class PtrOperator( Grammar ):
    grammar = OR( ( OPTIONAL( OPTIONAL( "::" ), NestedNameSpecifier ), "*", OPTIONAL( AttributeSpecifierSeq ), REPEAT( CvQualifier, min = 0 ) ),
                  ( OR( "&",
                        "&&" ), OPTIONAL( AttributeSpecifierSeq ) ) )

class ConversionFunctionId( Grammar ):
    grammar = "operator", TypeSpecifierSeq, REPEAT( PtrOperator, min = 0 )

class LiteralOperatorId( Grammar ):
    grammar = "operator", "\"", "\"", Identifier

class TemplateId( Grammar ):
    grammar = OR( SimpleTemplateId,
                  ( OR( OperatorFunctionId,
                        LiteralOperatorId ), "<", TemplateArgumentList, ">" ) )

class UnqualifiedId( Grammar ):
    grammar = OR( Identifier,
                  OperatorFunctionId,
                  ConversionFunctionId,
                  LiteralOperatorId,
                  ( "~", ClassName ),
                  ( "~", DecltypeSpecifier ),
                  TemplateId )

class QualifiedId( Grammar ):
    grammar = OR( ( OPTIONAL( "::" ), NestedNameSpecifier, OPTIONAL( "template"  ), UnqualifiedId ),
                  ( "::", OR( Identifier,
                              OperatorFunctionId,
                              LiteralOperatorId,
                              TemplateId ) ) )

class DeclaratorId( Grammar ):
    grammar = OR( ( OPTIONAL( "..." ), OR( UnqualifiedId,
                                           QualifiedId ) ),
                  ( OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), ClassName ) )

#class ParameterDeclarationList( Grammar ):
#    grammar = LIST_OF( ParameterDeclaration, sep = ",", collapse = True )

class ParameterDeclarationClause( Grammar ):
    #grammar = REPEAT( Token, min = 0, greedy = False, collapse = True )
    #grammar = OR( ( OPTIONAL( ParameterDeclarationList ), OPTIONAL( "..." ) ),
                  #( ParameterDeclarationList, ",", "..." ) )
    grammar = BALANCED_UNTIL_TOKENS( ")", True )

class RefQualifier( Grammar ):
    grammar = OR( "&",
                  "&&" )

#class TypeIdList( Grammar ):
    #grammar = LIST_OF( (TypeId, OPTIONAL( "..." ) ), sep = "," )

class DynamicExceptionSpecification( Grammar ):
    #grammar = "throw", "(", REPEAT( Token, greedy = False, min = 0, collapse = True ), ")"
    grammar = "throw", "(", BALANCED_TOKENS( "(", ")" ), ")"

class NoexceptSpecification( Grammar ):
    grammar = "noexcept", OPTIONAL( "(", BALANCED_TOKENS( "(", ")" ), ")" )

class ExceptionSpecification( Grammar ):
    grammar = OR( DynamicExceptionSpecification,
                  NoexceptSpecification )

class ParametersAndQualifiers( Grammar ):
    grammar = "(", ParameterDeclarationClause, ")", OPTIONAL( AttributeSpecifierSeq ), REPEAT( CvQualifier, min = 0 ), OPTIONAL( RefQualifier ), OPTIONAL( ExceptionSpecification )

class NoptrDeclarator( Grammar ):
    grammar = OR( ( DeclaratorId, OPTIONAL( AttributeSpecifierSeq ) ),
                  ( "(", REF( "PtrDeclarator" ), ")" ) ), REPEAT( OR( ParametersAndQualifiers,
                                                                      ( "[", BALANCED_TOKENS( "[", "]" ), "]", OPTIONAL( AttributeSpecifierSeq ) ) ), min = 0 )

class PtrDeclarator( Grammar ):
    grammar = OR( NoptrDeclarator,
                  ( PtrOperator, REF( "PtrDeclarator" ) ) )

class NoptrAbstractDeclarator( Grammar ):
    grammar = "(", REF( "PtrAbstractDeclarator" ), ")", REPEAT( OR( ParametersAndQualifiers,
                                                                    ( "[", BALANCED_TOKENS( "[", "]" ), "]", OPTIONAL( AttributeSpecifierSeq ) ) ) )

class PtrAbstractDeclarator( Grammar ):
    grammar = OR( NoptrAbstractDeclarator,
                  ( PtrOperator, OPTIONAL( REF( "PtrAbstractDeclarator" ) ) ) )

class AbstractDeclarator( Grammar ):
    grammar = OR( PtrAbstractDeclarator,
                  ( OPTIONAL( NoptrAbstractDeclarator ), ParametersAndQualifiers, REF( "TrailingReturnType" ) ),
                  "..." )

class TrailingReturnType( Grammar ):
    grammar = "->", REPEAT( TrailingTypeSpecifier ), OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( AbstractDeclarator )

class Declarator( Grammar ):
    grammar = OR( ( NoptrDeclarator, ParametersAndQualifiers, TrailingReturnType ),
                  PtrDeclarator )

class VirtSpecifier( Grammar ):
    grammar = OR( "override",
                  "final",
                  "new" )

class PureSpecifier( Grammar ):
    grammar = LITERAL( "=" ), LITERAL( "0" )

class BracedInitList( Grammar ):
    #grammar = "{", REPEAT( Token, min = 0, greedy = False, collapse = True ), "}"
    grammar = "{", BALANCED_TOKENS( "{", "}" ), "}"

class InitializerClause( Grammar ):
    grammar = OR( BracedInitList,
                  BALANCED_UNTIL_TOKENS( ( ";", "," ), True ) )

class BraceOrEqualInitializer( Grammar ):
    grammar = OR( ( "=", InitializerClause ),
                  BracedInitList )

class MemberDeclarator( Grammar ): #incomplete
    grammar = OR( ( Declarator, REPEAT( VirtSpecifier, min = 0 ), OPTIONAL( OR( PureSpecifier,
                                                                                     BraceOrEqualInitializer ) ) ),
                  ( OPTIONAL( Identifier ), OPTIONAL( AttributeSpecifierSeq ), REPEAT( VirtSpecifier, min = 0 ), ":", BALANCED_UNTIL_TOKENS( ( ";", "," ), True ) ) ) 

class UsingDeclaration( Grammar ):
    #grammar = "using", REPEAT( Token, greedy = False, collapse = True ), ";" 
    grammar = "using", BALANCED_UNTIL_TOKENS( ";" )

class StaticAssertDeclaration( Grammar ):
    #grammar = "static_assert", REPEAT( Token, greedy = False, collapse = True ), ";" 
    grammar = "static_assert", BALANCED_UNTIL_TOKENS( ";" )

class TemplateDeclaration( Grammar ):
    #grammar = "template", "<", REPEAT( Token, min = 0, collapse = True, greedy = False ), ">", REF( "Declaration" )
    grammar = "template", "<", BALANCED_UNTIL_TOKENS( ">", True ), ">", REF( "Declaration" )

class BalancedBraces( Grammar ):
    grammar = OR( EXCEPT( Token, "{" ),
                  ( "{", REPEAT( REF( "BalancedBraces" ), min = 0, collapse = True ), "}" ) )
    grammar_collapse = True

class MemberDeclaration( Grammar ):
    grammar = OR( ( OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( REF( "DeclSpecifierSeq" ) ), OPTIONAL( LIST_OF( MemberDeclarator, sep = "," ) ), ";" ),
                  ( REF( "FunctionDefinition" ), OPTIONAL( ";" ) ),
                  UsingDeclaration,
                  StaticAssertDeclaration,
                  TemplateDeclaration )

class ClassSpecifier( Grammar ):
    #grammar = ClassHead, "{", REPEAT( MemberSpecification, greedy = False, collapse = True, min = 0 ), "}"
    grammar = ClassHead, "{", BALANCED_TOKENS( "{", "}" ), "}"

    def ParseMembers( self ):
        member_grammar = REPEAT( OR( ( AccessSpecifier, ":" ),
                                     MemberDeclaration ), min = 0 )
        parser = member_grammar.parser()
        result = parser.parse_string( self.elements[2].string, reset = True, eof = True )
        self.elements = list( self.elements )
        self.elements[2] = result

class EnumKey( Grammar ):
    grammar = "enum", OPTIONAL( OR( "class",
                                    "struct" ) )

class EnumBase( Grammar ):
    grammar = ":", TypeSpecifierSeq

class EnumHead( Grammar ):
    grammar = EnumKey, OPTIONAL( AttributeSpecifierSeq ), OR( OPTIONAL( Identifier ),
                                                              ( NestedNameSpecifier, Identifier ) ), OPTIONAL( EnumBase )

class EnumeratorDefinition( Grammar ):
    grammar = Identifier, OPTIONAL( "=", BALANCED_UNTIL_TOKENS( ( ",", "}" ), True ) )

class EnumeratorList( Grammar ):
    grammar = LIST_OF( EnumeratorDefinition, sep = "," )

class EnumSpecifier( Grammar ):
    #grammar = EnumHead, "{", OPTIONAL( EnumeratorList, OPTIONAL( "," ) ), "}"
    grammar = EnumHead, "{", BALANCED_TOKENS( "{", "}" ), "}"

class TypeSpecifier( Grammar ):
    grammar = OR( TrailingTypeSpecifier,
                  ClassSpecifier,
                  EnumSpecifier )

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

class MemInitializer( Grammar ):
    grammar = ClassOrDecltype, OR( ( "(", BALANCED_TOKENS( "(", ")" ), ")" ),
                                   BracedInitList )

class CtorInitializer( Grammar ):
    grammar = ":", LIST_OF( MemInitializer, OPTIONAL( "..." ), sep = "," )

class FunctionBody( Grammar ):
    grammar = OPTIONAL( CtorInitializer ), "{", BALANCED_TOKENS( "{", "}" ), "}"

class FunctionDefinition( Grammar ):
    grammar = OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( DeclSpecifierSeq ), Declarator, OR( FunctionBody,
                                                                                               ( "=", OR( "default",
                                                                                                          "delete" ), ";" ) )

class Initializer( Grammar ):
    grammar = OR( BraceOrEqualInitializer,
                  ( "(", BALANCED_TOKENS( "(", ")" ), ")" ) )

class InitDeclaratorList( Grammar ):
    grammar = LIST_OF( Declarator, OPTIONAL( Initializer ), sep = "," )

class SimpleDeclaration( Grammar ):
    grammar = OPTIONAL( AttributeSpecifierSeq ), OPTIONAL( DeclSpecifierSeq ), OPTIONAL( InitDeclaratorList ), ";"

class AsmDefinition( Grammar ):
    grammar = "asm", "(", StringLiteral, ")"

class QualifiedNamespaceSpecifier( Grammar ):
    grammar = OPTIONAL( "::" ), OPTIONAL( NestedNameSpecifier ), Identifier

class NamespaceAliasDefinition( Grammar ):
    grammar = "namespace", Identifier, "=", QualifiedNamespaceSpecifier, ";"

class OpaqueEnumDeclaration( Grammar ):
    grammar = EnumKey, OPTIONAL( AttributeSpecifierSeq ), Identifier, OPTIONAL( EnumBase ), ";"

class BlockDeclaration( Grammar ):
    grammar = OR( SimpleDeclaration,
                  AsmDefinition,
                  NamespaceAliasDefinition,
                  UsingDeclaration,
                  StaticAssertDeclaration,
                  OpaqueEnumDeclaration )

class NamespaceDefinition( Grammar ):
    grammar = OPTIONAL( "inline" ), "namespace", OPTIONAL( Identifier ), "{", OPTIONAL( REF( "DeclarationSeq" ) ), "}"

class ExplicitInstantiation( Grammar ):
    grammar = OPTIONAL( "extern" ), "template", REF( "Declaration" )

class ExplicitSpecialization( Grammar ):
    grammar = "template", "<", ">", REF( "Declaration" )

class LinkageSpecification( Grammar ):
    grammar = "extern", StringLiteral, OR( ( "{", REF( "DeclarationSeq" ), "}" ),
                                           REF( "Declaration" ) )

class EmptyDeclaration( Grammar ):
    grammar = ";"

class AttributeDeclaration( Grammar ):
    grammar = AttributeSpecifierSeq, ";"

class Declaration( Grammar ):
    grammar = OR( FunctionDefinition,
                  BlockDeclaration,
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
    grammar = REPEAT( Declaration, collapse = True, min = 0 ), EOF

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
        stdout.write( element.string )
        #stdout.write( string[element.start:min(element.start + 20,element.end)] )
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
            for j in match.group():
                if j == "\n":
                    ret += "\n"
                else:
                    ret += " "
            i += len( match.group() )
            continue 

        #
        # Nothing has matched
        #
        ret += string[i]
        i += 1

    return ret

def ParseSubElements( element ):
    if not element:
        return
    if getattr( element, "ParseMembers", None ):
        element.ParseMembers()
    for e in element.elements:
        ParseSubElements( e )

def FormatSubElements( element ):
    if not element:
        return
    if getattr( element, "Format", None ):
        element.Format()
    for e in element.elements:
        FormatSubElements( e )

def main():
    #parser = BALANCED_UNTIL_TOKENS( ">", True ).parser()
    #string = "some template, paramr<with, words< here = ( 1 > 2) > >, end >"
    #result = parser.parse_string( string, reset = True, eof = True )
    #PrintElements( result )
    #exit()
    
    #TranslationUnit.grammar_resolve_refs( )
   
    #string = stdin.read()

    f = open( argv[1], "r" )
    original_string = f.read()

    string = RemoveComments( original_string )
    parser = TranslationUnit.parser()
    result = parser.parse_string( string, reset = True, eof = True )
    if not result:
        print( "Failed to parse" )
        return
    ParseSubElements( result )
    CalculateElementsRanges( result, string )
    FormatSubElements( result )
    PrintElementStrings( result, original_string )
    #print()
    #PrintElements( result )

if __name__ == "__main__":
    main()

