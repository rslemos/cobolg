package br.eti.rslemos.cobolg;

public class TextHelper {
	public static String join(String... lines) {
		StringBuilder builder = new StringBuilder();

		for (String line : lines) {
			builder.append(line).append('\n');
		}

		if (lines.length > 0)
			builder.setLength(builder.length() - 1);

		return builder.toString();
	}
}
