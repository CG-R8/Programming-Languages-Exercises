package edu.binghamton.cs571;

public class UglyRegexpParser {

	Token _lookahead;
	Scanner _scanner;

	UglyRegexpParser(Scanner scanner) {
		_scanner = scanner;
		_lookahead = _scanner.nextToken();
	}

	/**
	 * parse a sequence of lines containing ugly-regexp's; for each ugly regexp
	 * print out the corresponding standard regexp. If there is an error, print
	 * diagnostic and continue with next line.
	 */
	public void parse() {
		while (_lookahead.kind != Token.Kind.EOF) {
			try {
				String out = uglyRegexp();
				if (check(Token.Kind.NL))
					System.out.println("" + out);
				match(Token.Kind.NL);
			} catch (ParseException e) {
				System.err.println(e.getMessage());
				while (_lookahead.kind != Token.Kind.NL) {
					_lookahead = _scanner.nextToken();
				}
				_lookahead = _scanner.nextToken();
			}
		}
	}

	/**
	 * Return standard syntax regexp corresponding to ugly-regexp read from
	 * _scanner.
	 */
	// IMPLEMENT THIS FUNCTION and any necessary functions it may call.
	private String uglyRegexp() {

		String Character = term();
		return exprRest(Character);
	}

	private String exprRest(String character) {

		if (_lookahead.kind == Token.Kind.DOT) {
			match(Token.Kind.DOT);
			String character1 = term();
			return exprRest("(" + character + " " + character1 + ")");
		}
		// TODO Auto-generated method stub
		else if (_lookahead.kind == Token.Kind.PLUS) {
			match(Token.Kind.PLUS);
			String character1 = term();
			return exprRest("(" + character + "|" + character1 + ")");
		}

		else
			return character;
	}

	private String term() {
		// TODO Auto-generated method stub
		String character_term = factor();
		return termRest(character_term);
	}

	String termRest(String character_term) {
		// TODO Auto-generated method stub
		if (_lookahead.kind == Token.Kind.STAR) {
			match(Token.Kind.STAR);
			String character_termRest = factor();
			return termRest(character_term + character_termRest + "*");
		}
		return character_term;
	}

	private String factor() {
		// TODO Auto-generated method stub
		String character_simple = "";
		if (_lookahead.kind == Token.Kind.CHARS) {
			character_simple = _lookahead.lexeme;
			match(Token.Kind.CHARS);
			return chars_As_Entity(character_simple);
		}
		if (_lookahead.kind == Token.Kind.LPAREN) {
			match(Token.Kind.LPAREN);
			character_simple = uglyRegexp();
			character_simple = "(" + character_simple;
			match(Token.Kind.RPAREN);
			character_simple = character_simple + ")";
		}

		if (_lookahead.kind == Token.Kind.STAR) {
			match(Token.Kind.STAR);
			character_simple = factor();
			character_simple = character_simple + "*";

		}

		else if (_lookahead.kind == Token.Kind.CHAR) {
			character_simple = _lookahead.lexeme;
			match(Token.Kind.CHAR);
		} else {
			// syntaxError();

		}
		return character_simple;
	}

	private String chars_As_Entity(String character_simple2) {

		String character_simple = null;
		if (_lookahead.kind == Token.Kind.LPAREN) {
			match(Token.Kind.LPAREN);
		}
		character_simple = multiple_Alphabets(_lookahead.lexeme);
		match(Token.Kind.RPAREN);
		return "[" + character_simple + "]";

	}

	private String multiple_Alphabets(String character_simple2) {
		// TODO Auto-generated method stub
		String character_malpha = single_Alphabet();
		return multiple_Alphabets_Rest(character_malpha);
	}

	private String multiple_Alphabets_Rest(String character_malpha) {
		String character_simple = null;
		if (_lookahead.kind == Token.Kind.COMMA) {
			// character_simple = _lookahead.lexeme;
			match(Token.Kind.COMMA);
			// TODO
			character_simple = single_Alphabet();
			return multiple_Alphabets_Rest(character_malpha + " " + character_simple + "");
		}
		return (character_malpha);

	}

	private String single_Alphabet() {
		// TODO Auto-generated method stub

		if (_lookahead.kind == Token.Kind.CHAR) {
			String character_simple = quote(_lookahead.lexeme);
			match(Token.Kind.CHAR);

			return (character_simple);
		}

		else if (_lookahead.kind == Token.Kind.CHAR) {
			String character_simple = _lookahead.lexeme;
			match(Token.Kind.CHAR);
			return (character_simple);
		}

		else if (_lookahead.kind == Token.Kind.COMMA) {
			String character_simple = _lookahead.lexeme;
			match(Token.Kind.COMMA);
			return ("\\" + character_simple);
		} else if (_lookahead.kind == Token.Kind.DOT) {
			String character_simple = _lookahead.lexeme;
			match(Token.Kind.DOT);
			return ("\\" + character_simple);
		} else if (_lookahead.kind == Token.Kind.STAR) {
			String character_simple = _lookahead.lexeme;
			match(Token.Kind.STAR);
			return ("\\" + character_simple);
		} else if (_lookahead.kind == Token.Kind.LPAREN) {
			String character_simple = "\\" + _lookahead.lexeme;
			match(Token.Kind.LPAREN);
			return (character_simple);
		} else if (_lookahead.kind == Token.Kind.RPAREN) {
			String character_simple = "\\" + _lookahead.lexeme;
			match(Token.Kind.RPAREN);
			return (character_simple);
		} else if (_lookahead.kind == Token.Kind.PLUS) {
			String character_simple = "\\" + _lookahead.lexeme;
			match(Token.Kind.PLUS);
			return (character_simple);
		} else {
			syntaxError();
			return "ERROR";
		}
	}

	private void syntaxError() {
		System.out.println("Here is the syntax error");

		String message = String.format("%s: syntax error at %s", _lookahead.coords, _lookahead.lexeme);
		System.err.print("" + message);

		while (_lookahead.kind != Token.Kind.NL && _lookahead.kind != Token.Kind.EOF) {
			_lookahead = _scanner.nextToken();
		}
	}

	/**
	 * Return s with first char escaped using a '\' if it is non-alphanumeric.
	 */
	private static String quote(String s) {
		return (Character.isLetterOrDigit(s.charAt(0))) ? s : "\\" + s;
	}

	/** Return true iff _lookahead.kind is equal to kind. */
	private boolean check(Token.Kind kind) {
		return check(kind, null);
	}

	/**
	 * Return true iff lookahead kind and lexeme are equal to corresponding
	 * args. Note that if lexeme is null, then it is not used in the match.
	 */
	private boolean check(Token.Kind kind, String lexeme) {
		return (_lookahead.kind == kind && (lexeme == null || _lookahead.lexeme.equals(lexeme)));
	}

	/**
	 * If lookahead kind is equal to kind, then set lookahead to next token;
	 * else throw a ParseException.
	 */
	private void match(Token.Kind kind) {
		match(kind, null);
	}

	/**
	 * If lookahead kind and lexeme are not equal to corresponding args, then
	 * set lookahead to next token; else throw a ParseException. Note that if
	 * lexeme is null, then it is not used in the match.
	 */
	private void match(Token.Kind kind, String lexeme) {
		if (check(kind, lexeme)) {
			_lookahead = _scanner.nextToken();
		} else {
			String expected = (lexeme == null) ? kind.toString() : lexeme;
			String message = String.format("%s: syntax error at '%s', expecting '%s'", _lookahead.coords,
					_lookahead.lexeme, expected);
			throw new ParseException(message);
		}
	}

	private static class ParseException extends RuntimeException {
		ParseException(String message) {
			super(message);
		}
	}

	/**
	 * main program: parses and translates ugly-regexp's contained in the file
	 * specified by it's single command-line argument.
	 */
	public static void main(String[] args) {
		if (args.length != 1) {
			System.err.format("usage: java %s FILENAME\n", UglyRegexpParser.class.getName());
			System.exit(1);
		}
		Scanner scanner = ("-".equals(args[0])) ? new Scanner() : new Scanner(args[0]);
		(new UglyRegexpParser(scanner)).parse();
	}

}
