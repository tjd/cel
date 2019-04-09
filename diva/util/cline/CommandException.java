package util.cline;

public class CommandException extends RuntimeException {

	String error_;

	public CommandException(String error) {
		error_ = error;
	}

	public String toString() {
		return error_;
	}
}
