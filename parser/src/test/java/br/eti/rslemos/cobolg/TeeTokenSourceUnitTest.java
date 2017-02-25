/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2015  Rodrigo Lemos
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * END COPYRIGHT NOTICE
 ******************************************************************************/
package br.eti.rslemos.cobolg;

import static com.google.common.base.Predicates.and;
import static com.google.common.base.Predicates.compose;
import static com.google.common.base.Predicates.instanceOf;
import static com.google.common.base.Predicates.not;
import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.lang.ref.Reference;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import objectexplorer.Chain;
import objectexplorer.ObjectExplorer;
import objectexplorer.ObjectVisitor;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenFactory;
import org.antlr.v4.runtime.ListTokenSource;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenFactory;
import org.antlr.v4.runtime.TokenSource;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.misc.Pair;
import org.junit.Test;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;

public class TeeTokenSourceUnitTest {
	private final Pair<TokenSource, CharStream> pair = new Pair<TokenSource, CharStream>(null, null);
	
	private final Token T1 = CommonTokenFactory.DEFAULT.create(pair, 1, "T1", Token.DEFAULT_CHANNEL,  0,  2, 1, 0);
	private final Token T2 = CommonTokenFactory.DEFAULT.create(pair, 1, "T2", Token.HIDDEN_CHANNEL,   2,  4, 1, 2);
	private final Token T3 = CommonTokenFactory.DEFAULT.create(pair, 1, "T3", Token.DEFAULT_CHANNEL,  4,  6, 1, 4);
	private final Token T4 = CommonTokenFactory.DEFAULT.create(pair, 1, "T4", Token.HIDDEN_CHANNEL,   6,  8, 2, 0);
	private final Token T5 = CommonTokenFactory.DEFAULT.create(pair, 1, "T5", Token.DEFAULT_CHANNEL,  8, 10, 2, 2);

	private final List<Token> tokens = new ArrayList<Token>(asList(T1, T2, T3, T4, T5));
	private TokenSource source = new ListTokenSource(tokens, "list of tokens");

	private TeeTokenSource tee = new TeeTokenSource(source);
	private TokenSource channel1 = tee.splitChannel();

	@Test public void returnEOFOnDefaultChannelFromEmptyTokenSource() {
		tokens.clear();
		
		assertThat(channel1.nextToken().getType(), is(equalTo(Token.EOF)));
		assertThat(channel1.nextToken().getType(), is(equalTo(Token.EOF)));
		assertThat(channel1.nextToken().getType(), is(equalTo(Token.EOF)));
	}

	@Test public void multipleCopiesOfAChannel() {
		TokenSource channel2 = tee.splitChannel();
		
		assertThat(channel1.nextToken(), is(sameInstance(T1)));
		assertThat(channel1.nextToken(), is(sameInstance(T2)));
		assertThat(channel1.nextToken(), is(sameInstance(T3)));
		assertThat(channel1.nextToken(), is(sameInstance(T4)));
		assertThat(channel1.nextToken(), is(sameInstance(T5)));
		assertThat(channel1.nextToken().getType(), is(equalTo(Token.EOF)));
		assertThat(channel1.nextToken().getType(), is(equalTo(Token.EOF)));
		
		assertThat(channel2.nextToken(), is(sameInstance(T1)));
		assertThat(channel2.nextToken(), is(sameInstance(T2)));
		assertThat(channel2.nextToken(), is(sameInstance(T3)));
		assertThat(channel2.nextToken(), is(sameInstance(T4)));
		assertThat(channel2.nextToken(), is(sameInstance(T5)));
		assertThat(channel2.nextToken().getType(), is(equalTo(Token.EOF)));
		assertThat(channel2.nextToken().getType(), is(equalTo(Token.EOF)));
	}

	@Test public void lateCopyOfAChannel() {
		assertThat(channel1.nextToken(), is(sameInstance(T1)));
		assertThat(channel1.nextToken(), is(sameInstance(T2)));
		assertThat(channel1.nextToken(), is(sameInstance(T3)));

		TokenSource channel2 = tee.splitChannel();
		
		assertThat(channel1.nextToken(), is(sameInstance(T4)));
		assertThat(channel1.nextToken(), is(sameInstance(T5)));
		assertThat(channel1.nextToken().getType(), is(equalTo(Token.EOF)));
		assertThat(channel1.nextToken().getType(), is(equalTo(Token.EOF)));
		
		// T1, T2, T3 are already spent
		assertThat(channel2.nextToken(), is(sameInstance(T4)));
		assertThat(channel2.nextToken(), is(sameInstance(T5)));
		assertThat(channel2.nextToken().getType(), is(equalTo(Token.EOF)));
		assertThat(channel2.nextToken().getType(), is(equalTo(Token.EOF)));
	}

	@Test public void earlyDisconnectionOfAChannel() {
		TokenSource channel2 = tee.splitChannel();
		
		assertThat(channel1.nextToken(), is(sameInstance(T1)));
		assertThat(channel1.nextToken(), is(sameInstance(T2)));
		assertThat(channel1.nextToken(), is(sameInstance(T3)));

		tee.disconnect(channel2);
		
		assertThat(channel1.nextToken(), is(sameInstance(T4)));
		assertThat(channel1.nextToken(), is(sameInstance(T5)));
		assertThat(channel1.nextToken().getType(), is(equalTo(Token.EOF)));
		assertThat(channel1.nextToken().getType(), is(equalTo(Token.EOF)));
		
		assertThat(channel2.nextToken(), is(sameInstance(T1)));
		assertThat(channel2.nextToken(), is(sameInstance(T2)));
		assertThat(channel2.nextToken(), is(sameInstance(T3)));
		
		assertThat(channel2.nextToken().getType(), is(equalTo(Token.EOF)));
		assertThat(channel2.nextToken().getType(), is(equalTo(Token.EOF)));
	}

	@Test public void testGetLine() {
		assertThat(channel1.getLine(), is(equalTo(T1.getLine())));
		assertThat(channel1.nextToken(), is(sameInstance(T1)));
		assertThat(channel1.getLine(), is(equalTo(T2.getLine())));
		assertThat(channel1.nextToken(), is(sameInstance(T2)));
		assertThat(channel1.getLine(), is(equalTo(T3.getLine())));
		assertThat(channel1.nextToken(), is(sameInstance(T3)));
		assertThat(channel1.getLine(), is(equalTo(T4.getLine())));
		assertThat(channel1.nextToken(), is(sameInstance(T4)));
		assertThat(channel1.getLine(), is(equalTo(T5.getLine())));
		assertThat(channel1.nextToken(), is(sameInstance(T5)));
	}

	@Test public void testGetCharPositionInLine() {
		assertThat(channel1.getCharPositionInLine(), is(equalTo(T1.getCharPositionInLine())));
		assertThat(channel1.nextToken(), is(sameInstance(T1)));
		assertThat(channel1.getCharPositionInLine(), is(equalTo(T2.getCharPositionInLine())));
		assertThat(channel1.nextToken(), is(sameInstance(T2)));
		assertThat(channel1.getCharPositionInLine(), is(equalTo(T3.getCharPositionInLine())));
		assertThat(channel1.nextToken(), is(sameInstance(T3)));
		assertThat(channel1.getCharPositionInLine(), is(equalTo(T4.getCharPositionInLine())));
		assertThat(channel1.nextToken(), is(sameInstance(T4)));
		assertThat(channel1.getCharPositionInLine(), is(equalTo(T5.getCharPositionInLine())));
		assertThat(channel1.nextToken(), is(sameInstance(T5)));
	}
	
	private void spySource() {
		source = spy(source);
		tee = new TeeTokenSource(source);
		channel1 = tee.splitChannel();
	}

	@Test public void shouldAlwaysDelegateGetInputStreamAnswer() {
		class NullCharStream implements CharStream {
			@Override public void consume() { }

			@Override public int LA(int i) { return 0; }

			@Override public int mark() { return 0; }

			@Override public void release(int marker) { }

			@Override public int index() { return 0; }

			@Override public void seek(int index) { }

			@Override public int size() { return 0; }

			@Override public String getSourceName() { return null; }

			@Override public String getText(Interval interval) { return null; }
		}
		
		spySource();
		
		// each call is delegated
		verify(source, never()).getInputStream();
		channel1.getInputStream();
		verify(source, times(1)).getInputStream();
		channel1.getInputStream();
		verify(source, times(2)).getInputStream();
		channel1.getInputStream();
		verify(source, times(3)).getInputStream();
		channel1.getInputStream();
		verify(source, times(4)).getInputStream();
		
		// the result is never transformed
		{
			CharStream cs = new NullCharStream();
			when(source.getInputStream()).thenReturn(cs);
			
			assertThat(channel1.getInputStream(), is(sameInstance(cs)));
			verify(source, times(5)).getInputStream();
		}
		
		verifyNoMoreInteractions(source);
	}

	@Test public void shouldAlwaysDelegateGetSourceName() {
		spySource();
		
		// each call is delegated
		verify(source, never()).getSourceName();
		channel1.getSourceName();
		verify(source, times(1)).getSourceName();
		channel1.getSourceName();
		verify(source, times(2)).getSourceName();
		channel1.getSourceName();
		verify(source, times(3)).getSourceName();
		channel1.getSourceName();
		verify(source, times(4)).getSourceName();
		
		// the result is never transformed
		{
			String s = new String("surely a new instance; never interned");
			when(source.getSourceName()).thenReturn(s);

			assertThat(channel1.getSourceName(), is(sameInstance(s)));
			verify(source, times(5)).getSourceName();
		}
		
		verifyNoMoreInteractions(source);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test public void shouldAlwaysDelegateGetTokenFactory() {
		class NullTokenFactory<T extends Token> implements TokenFactory<T> {
			@Override public T create(Pair<TokenSource, CharStream> source, int type, String text, int channel, int start, int stop, int line, int charPositionInLine) { return null; }
			
			@Override public T create(int type, String text) { return null; }
		}
		
		spySource();
		
		// each call is delegated
		verify(source, never()).getTokenFactory();
		channel1.getTokenFactory();
		verify(source, times(1)).getTokenFactory();
		channel1.getTokenFactory();
		verify(source, times(2)).getTokenFactory();
		channel1.getTokenFactory();
		verify(source, times(3)).getTokenFactory();
		channel1.getTokenFactory();
		verify(source, times(4)).getTokenFactory();
		
		// the result is never transformed
		{
			TokenFactory tf = new NullTokenFactory();
			when(source.getTokenFactory()).thenReturn(tf);

			assertThat(channel1.getTokenFactory(), is(sameInstance(tf)));
			verify(source, times(5)).getTokenFactory();
		}
		
		verifyNoMoreInteractions(source);
	}
	
	@SuppressWarnings({ "rawtypes" })
	@Test public void shouldAlwaysDelegateSetTokenFactory() {
		class NullTokenFactory<T extends Token> implements TokenFactory<T> {
			@Override public T create(Pair<TokenSource, CharStream> source, int type, String text, int channel, int start, int stop, int line, int charPositionInLine) { return null; }
			
			@Override public T create(int type, String text) { return null; }
		}
		
		spySource();

		// each call is delegated and the argument is never transformed
		verify(source, never()).setTokenFactory(null);
		
		{
			TokenFactory tf = new NullTokenFactory();

			channel1.setTokenFactory(tf);
			verify(source).setTokenFactory(same(tf));
		}
		
		{
			TokenFactory tf = new NullTokenFactory();

			channel1.setTokenFactory(tf);
			verify(source).setTokenFactory(same(tf));
		}
		
		{
			TokenFactory tf = new NullTokenFactory();

			channel1.setTokenFactory(tf);
			verify(source).setTokenFactory(same(tf));
		}
		
		{
			TokenFactory tf = new NullTokenFactory();

			channel1.setTokenFactory(tf);
			verify(source).setTokenFactory(same(tf));
		}
		
		verifyNoMoreInteractions(source);
	}
	
	@Test public void unwatchedTokensShouldBeDiscarded() {
		TokenSource source;
		
		{
			// tokens are created anew...
			final Token T[] = {
				CommonTokenFactory.DEFAULT.create(pair, 1, "T01", Token.DEFAULT_CHANNEL,  0,  2, 1, 0),
				CommonTokenFactory.DEFAULT.create(pair, 1, "T02", Token.HIDDEN_CHANNEL,   2,  4, 1, 2),
				CommonTokenFactory.DEFAULT.create(pair, 1, "T03", Token.DEFAULT_CHANNEL,  4,  6, 1, 4),
				CommonTokenFactory.DEFAULT.create(pair, 1, "T04", Token.HIDDEN_CHANNEL,   6,  8, 2, 0),
				CommonTokenFactory.DEFAULT.create(pair, 1, "T05", Token.DEFAULT_CHANNEL,  8, 10, 2, 2),
			};
			
			// ...stored inside mock...
			source = new TokenSource() {
				int next = 0;

				@Override public Token nextToken() {
					try {
						return T[next];
					} finally {
						// ...then forgotten...
						T[next++] = null;
					}
				}

				@Override public int getLine() { return 0; }

				@Override public int getCharPositionInLine() { return 0; }

				@Override public CharStream getInputStream() { return null; }

				@Override public String getSourceName() { return null; }

				@Override public void setTokenFactory(TokenFactory<?> factory) { }

				@Override public TokenFactory<?> getTokenFactory() { return null; }
				
			};
			
			// ...then their strong references discarded
		}

		TeeTokenSource tee = new TeeTokenSource(source);
		TokenSource channel1 = tee.splitChannel();
		tee.splitChannel();

		System.gc(); Thread.yield();
		System.gc(); Thread.yield();
		System.gc(); Thread.yield();
		
		// simulates current thread stack frame
		Object[] frame = { source, tee, channel1 };

		{
			Set<Token> reachable = collectStronglyReachable(Token.class, frame);
			assertThat(reachable.size(), is(equalTo(5)));
		}
		
		channel1.nextToken(); // spends T1
		{
			Set<Token> reachable = collectStronglyReachable(Token.class, frame);
			assertThat(reachable.size(), is(equalTo(4)));
		}
		
		channel1.nextToken(); // spends T2
		{
			Set<Token> reachable = collectStronglyReachable(Token.class, frame);
			assertThat(reachable.size(), is(equalTo(3)));
		}
		
		channel1.nextToken(); // spends T3
		{
			Set<Token> reachable = collectStronglyReachable(Token.class, frame);
			assertThat(reachable.size(), is(equalTo(2)));
		}
		
		channel1.nextToken(); // spends T4
		{
			Set<Token> reachable = collectStronglyReachable(Token.class, frame);
			assertThat(reachable.size(), is(equalTo(1)));
		}
		
		channel1.nextToken(); // spends T5
		{
			Set<Token> reachable = collectStronglyReachable(Token.class, frame);
			assertThat(reachable.size(), is(equalTo(0)));
		}
	}

	@SuppressWarnings("unchecked")
	private <T> Set<T> collectStronglyReachable(Class<T> clazz, Object[] frame) {
		Set<T> objects = ObjectExplorer.exploreObject(frame, new Collector<T>(clazz, and(
				// only strong references
				compose(not(instanceOf(Reference.class)), GetValue.INSTANCE),
				// avoid leaking the whole test (because it has token fields)
				compose(not(Predicates.equalTo((Object)this)), GetValue.INSTANCE),
				// cut loops
				new ObjectExplorer.AtMostOncePredicate()
			)));
		return objects;
	}
}

class Collector<T> implements ObjectVisitor<Set<T>> {
	private final Class<T> clazz;
	private final Predicate<Chain> predicate;

	private final Map<T, Object> objects = new IdentityHashMap<T, Object>();

	public Collector(Class<T> clazz, Predicate<Chain> predicate) {
		this.clazz = clazz;
		this.predicate = predicate;
	}
	
	@SuppressWarnings("unchecked")
	@Override public Traversal visit(Chain chain) {
		if (!predicate.apply(chain))
			return Traversal.SKIP;
	
		if (clazz.isInstance(chain.getValue()))
			objects.put((T) chain.getValue(), null);

		return Traversal.EXPLORE;
	}

	@Override public Set<T> result() {
		return objects.keySet();
	}
}

class GetValue implements Function<Chain, Object> {
	public static final GetValue INSTANCE = new GetValue();
	
	@Override public Object apply(Chain input) {
		return input.getValue();
	}
}