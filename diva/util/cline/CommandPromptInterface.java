package util.cline;

public interface CommandPromptInterface {
	public void setRegister(CommandRegister cregister);
	public void start();
	void print(String string);
	void println(String string);
	void print(Object object);
	void println(Object object);
	public void message(String string);
	public String askQuestion(String string);
	public String askQuestion(String string, String defaultAnswer);
	public boolean askBooleanQuestion(String string);
}
