package main

import (
	"fmt"
	"strconv"
)

type TokenType int32

const (
	NUMBER    TokenType = iota
	ADD_SYM   TokenType = iota
	MUL_SYM   TokenType = iota
	LEFT_PAR  TokenType = iota
	RIGHT_PAR TokenType = iota
	EOL       TokenType = iota
)

type Token struct {
	tokenType TokenType
	value     interface{}
}

var tokenStrings = map[TokenType]string{
	NUMBER:    "Number",
	ADD_SYM:   "Add",
	MUL_SYM:   "Mul",
	LEFT_PAR:  "LPar",
	RIGHT_PAR: "RPar",
	EOL:       "EOL",
}

func (t TokenType) String() string {
	if s, ok := tokenStrings[t]; ok {
		return s
	}
	panic("Invalid Token")
}

func (t Token) String() string {
	if t.tokenType == NUMBER {
		return fmt.Sprintf("(%v, %v)", t.tokenType, t.value.(int))
	}
	return t.tokenType.String()
}

func isNumber(c byte) bool {
	return c >= '0' && c <= '9'
}

func lex(input string) []Token {
	tokens := make([]Token, 0)
	pos := 0
	for pos < len(input) {
		c := input[pos]
		switch c {
		case '+':
			tokens = append(tokens, Token{ADD_SYM, nil})
		case '*':
			tokens = append(tokens, Token{MUL_SYM, nil})
		case '(':
			tokens = append(tokens, Token{LEFT_PAR, nil})
		case ')':
			tokens = append(tokens, Token{RIGHT_PAR, nil})
		default:
			if isNumber(c) {
				start := pos
				for pos+1 < len(input) && isNumber(input[pos+1]) {
					pos++
				}
				value, _ := strconv.Atoi(input[start : pos+1])
				tokens = append(tokens, Token{NUMBER, value})
			} else if c != ' ' {
				fmt.Printf("Unknown type: %c\n", c)
			}
		}
		pos++
	}
	return append(tokens, Token{EOL, nil})
}

type Expression interface {
	Evaluate() int
}

type Number struct {
	value int
}

func (n Number) Evaluate() int {
	return n.value
}

type Addition struct {
	lhs Expression
	rhs Expression
}

func (a Addition) Evaluate() int {
	return a.lhs.Evaluate() + a.rhs.Evaluate()
}

type Multiplication struct {
	lhs Expression
	rhs Expression
}

func (a Multiplication) Evaluate() int {
	return a.lhs.Evaluate() * a.rhs.Evaluate()
}

type BindingPower struct {
	left  uint8
	right uint8
}

func infixBindingPower(tt TokenType) BindingPower {
	switch tt {
	case ADD_SYM:
		return BindingPower{1, 2}
	case MUL_SYM:
		return BindingPower{3, 4}
	default:
		panic("No binding power for token type")
	}
}

func parse(tokens []Token, min_bp uint8) (Expression, []Token) {
	var lhs Expression
	var rhs Expression
	lhs = Number{tokens[0].value.(int)}
	for {
		op := tokens[1].tokenType
		if op == EOL {
			break
		}
		bp := infixBindingPower(op)
		if bp.left < min_bp {
			break
		}
		rhs, tokens = parse(tokens[2:], bp.right)
		switch op {
		case ADD_SYM:
			lhs = Addition{lhs, rhs}
		case MUL_SYM:
			lhs = Multiplication{lhs, rhs}
		}
	}
	return lhs, tokens
}

func main() {
	tokens := lex("3 + 4 * 4 + 9 + 2 * 3")
	fmt.Printf("%v\n", tokens)
	expr, _ := parse(tokens, 0)
	fmt.Printf("%v\n", expr.Evaluate())
}
