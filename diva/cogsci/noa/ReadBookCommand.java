// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import util.cline.BaseCommand;
import cogsci.Control;
import java.util.*;
import java.io.*;

/**
 * This command reads in notes that you take when you read a book.
 * It _tries to generate a conceptual map that will organize the ideas
 * that you took notes about.
 */
public class ReadBookCommand extends BaseCommand {
	
	private final String TITLE = "TITLE";
	private final String AUTHOR = "AUTHOR";
		
	Hub titleHub_ = null;
	Hub authorHub_ = null;
	Hub quoteHub_ = null;
	Hub bookHub_ = null;

	int lineNumber_ = 0;
	int lastPageNumber_ = 0;

	class ReadBookException extends RuntimeException {
		ReadBookException(String problem) {
			super(problem);
		}

		public String toString() {
			return "on line #" + lineNumber_ + ": " + super.toString();
		}
	}

	public ReadBookCommand() {
		super("readbook", 1, "fileName" +
			"\n - reads a file containing a book summary");
	}

	public void execute(String args[]) {
		String fileName = args[0];
		BufferedReader in = null;
		
		System.out.println("reading book: " + args[0]);

		if (bookHub_ == null) {
			String book = "book";
			bookHub_ = NOA.get(book);
			if (bookHub_ == null) {
				bookHub_ = new Hub("book");
			}
		}

		try {
			in = new BufferedReader(new FileReader(fileName));
		} catch (FileNotFoundException e) {
			Control.error("file not found: " + fileName);
			return;
		}

		String nextLine = null;
		lineNumber_ = 0;
		StringBuffer quote = new StringBuffer(400);
		
		try {
			while ((nextLine=in.readLine()) != null) {
				lineNumber_++;
				if (isBlankLine(nextLine)) {
					System.out.println("blank line: " + lineNumber_);
					parseQuote(quote.toString());
					quote.delete(0, quote.length());
					continue;
				} else if (nextLine.startsWith(TITLE)) {
					parseTitle(nextLine);
				} else if (nextLine.startsWith(AUTHOR)) {
					parseAuthor(nextLine);
				} else {
					quote.append(nextLine + "\n");
				}
			}
			in.close();
		} catch (ReadBookException e) {
			Control.error(e);
		} catch (IOException e) {
			Control.error("IO exception reading book file " + fileName);
		}
	}

	final boolean isBlankLine(String line) {
		if (line.equals("") || line.equals("\n") || line.equals("\r")) {
			return true;
		} else if (line.startsWith("#")) {
			return true;
		} else if (line.startsWith(" ") || line.startsWith("\t")) {
			for (int i=0; i< line.length(); i++) {
				char next = line.charAt(i);
				if (next != ' ' && next != '\t') {
					return false;
				}
			}
			return true;
		}
		return false;
	}
	
	final String formatString(String input) {
		char array[] = input.toCharArray();
		char newArray[] = new char[array.length];
		int j=0;
		for (int i=0; i<array.length; i++) {
			if (array[i] != '_') {
				newArray[j] = array[i];
				j++;
			} 
		}
		return (new String(newArray)).trim();
	}

	final void parseQuote(String quote) {
		if (titleHub_ == null) {
			Control.error("The title of the book must be the first thing " +
				"specified with the 'TITLE' keyword, for example: \n" +
				"TITLE \"The Wordly Philosophers\"");
		}
		System.out.println("parsing quote");
		quoteHub_ = new TextBlob("quote", formatString(quote));
		quoteHub_.associate(bookHub_, titleHub_);
		addWordsToNOA(quote, quoteHub_);
	}

	final void parseTitle(String title) {
		String newTitle = title.substring(TITLE.length());
		titleHub_ = new Hub(formatString(newTitle));
		titleHub_.associate(NOA.IS, bookHub_);
		addWordsToNOA(newTitle, titleHub_);
		if (authorHub_ != null) {
			titleHub_.associate(NOA.FROM, authorHub_);
		}
	}

	final void parseAuthor(String author) {
		String newAuthor = author.substring(AUTHOR.length());
		authorHub_ = new Hub(formatString(newAuthor));
		addWordsToNOA(newAuthor, authorHub_);
		if (titleHub_ != null) {
			authorHub_.associate(NOA.FROM, titleHub_);
		}
	}

	final void addWordsToNOA(String input, Hub associationHub) {
		Enumeration tokens = parseString(input);
		addWordsToNOA(tokens, associationHub);
	}
	
	final void addWordsToNOA(Enumeration tokens, Hub associationHub) {
		while (tokens.hasMoreElements()) {
			String token = (String)tokens.nextElement();
			if (token.startsWith("^")) {
				token = token.substring(1, token.length());
				Hub quoteTokenHub = new Hub(formatString(token));
				quoteTokenHub.associate(NOA.FROM, associationHub);
			} else if (token.startsWith("_")) {
				Hub tokenHub = new Hub(formatString(token));
				tokenHub.associate(NOA.FROM, associationHub);
			}

			if (token.indexOf(" ") != -1) {
				Enumeration innerTokens = parseStringNoQuotes(token);
				addWordsToNOA(innerTokens, associationHub);
			}
		}
	}
			
	final Enumeration parseString(String input) {
		if (input.indexOf('"') == -1) {
			return parseStringNoQuotes(input);
		}

		// else we might have to parse between quotations
		// For example, the string below: 
		// question "how are you" 2 today was nice
		// contains three tokens (question, how are you, 2)

		int curIndex = 0;
		int endIndex = input.length();

		// used to hold the tokens of the string
		Vector vector = new Vector(20);	

		while (curIndex < endIndex) {
			int spaceIndex = input.indexOf(' ', curIndex);
			int quotationIndex = input.indexOf('"', curIndex);

			//System.out.println("spaceIndex: [" + spaceIndex +
			//	"] quotationIndex: [" + quotationIndex +
			//	"] curIndex: [" + curIndex + "]");

			String next = null;
			if (quotationIndex == -1) {
				Enumeration enum = parseStringNoQuotes(
					input.substring(curIndex, endIndex));
				while (enum.hasMoreElements()) {
					vector.add(enum.nextElement());
				}
				break;
			} else if (spaceIndex != -1 && spaceIndex < quotationIndex) {
				next = input.substring(curIndex, spaceIndex);
				curIndex = spaceIndex+1;
			} else {	// quotationIndex != -1
				// we have to extract a string between quotations
				int matchingQuote =	input.indexOf('"', quotationIndex+1);
				if (matchingQuote == -1) {
					throw new ReadBookException("quotations do not match");
				}
				String betweenQuotes = input.substring(quotationIndex+1,
					matchingQuote);
				if (input.charAt(quotationIndex-1) == '_') {
					System.out.println(
						"quotations should be stored with hub");
					next = "^" + betweenQuotes;
				} else {
					next = betweenQuotes;
				}

				// this assumes that an end quote will always be followed
				// by a space
				curIndex = matchingQuote+2;
			}
			vector.add(next);
		}
		return vector.elements();
	}

	final Enumeration parseStringNoQuotes(String input) {
		StringTokenizer tokenizer = new StringTokenizer(input, " \t()"); 
		return (Enumeration)tokenizer;
	}
}

