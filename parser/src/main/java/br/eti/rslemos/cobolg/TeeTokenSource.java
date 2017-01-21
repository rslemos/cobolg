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

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.Set;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenFactory;
import org.antlr.v4.runtime.TokenSource;

public class TeeTokenSource {

	private TokenSource underlying;
	
	private final Set<WeakReference<TokenLiveBuffer>> bufferRefs = new HashSet<WeakReference<TokenLiveBuffer>>();
	private final ReferenceQueue<TokenLiveBuffer> staleRefs = new ReferenceQueue<TokenLiveBuffer>();

	private Token eof;

	public TeeTokenSource(TokenSource underlying) {
		this.underlying = underlying;
	}

	public TokenSource splitChannel() {
		TokenLiveBuffer buffer = new TokenLiveBuffer();
		bufferRefs.add(new WeakReference<TokenLiveBuffer>(buffer, staleRefs));
		
		return new InternalTokenSource(buffer);
	}
	
	public void disconnect(TokenSource t) {
		InternalTokenSource t2 = (InternalTokenSource) t;
		
		WeakReference<TokenLiveBuffer> bufferRef = getRef(t2.buffer);
		if (bufferRef == null)
			throw new IllegalStateException();
		
		bufferRefs.remove(bufferRef);
	}

	private WeakReference<TokenLiveBuffer> getRef(Deque<Token> buffer) {
		for (WeakReference<TokenLiveBuffer> bufferRef : bufferRefs)
			if (bufferRef.get() == buffer)
				return bufferRef;
		
		return null;
	}
	
	private void ensureNotEmpty(Deque<Token> buffer) {
		purgeStaleReferences();
		
		offerForAll(underlying.nextToken());
		if (buffer.isEmpty())
			// disconnected buffer
			offerEOF(buffer);
	}

	private void offerForAll(Token t) {
		if (t.getType() == Token.EOF)
			eof = t;
		
		for (WeakReference<TokenLiveBuffer> bufferRef : bufferRefs)
			bufferRef.get().offer(t);
	}

	private void offerEOF(Deque<Token> buffer) {
		if (eof != null)
			buffer.offer(eof);
		else
			buffer.offer(underlying.getTokenFactory().create(Token.EOF, "."));
	}

	private void purgeStaleReferences() {
		Reference<? extends TokenLiveBuffer> ref;
		while ((ref = staleRefs.poll()) != null)
			bufferRefs.remove(ref);
	}
	
	@SuppressWarnings("serial")
	private class TokenLiveBuffer extends ArrayDeque<Token> {
		@Override public Token peek() {
			ensureNotEmpty(this);
			return super.peek();
		}

		@Override public Token pop() {
			ensureNotEmpty(this);
			return super.pop();
		}
	};

	private class InternalTokenSource implements TokenSource {
		private final Deque<Token> buffer;
		
		private InternalTokenSource(Deque<Token> buffer) {
			this.buffer = buffer;
		}

		@Override
		public Token nextToken() {
			return buffer.pop();
		}

		@Override
		public int getLine() {
			return buffer.peek().getLine();
		}

		@Override
		public int getCharPositionInLine() {
			return buffer.peek().getCharPositionInLine();
		}
		
		@Override
		public CharStream getInputStream() {
			return underlying.getInputStream();
		}

		@Override
		public String getSourceName() {
			return underlying.getSourceName();
		}

		@Override
		public void setTokenFactory(TokenFactory<?> factory) {
			underlying.setTokenFactory(factory);
		}

		@Override
		public TokenFactory<?> getTokenFactory() {
			return underlying.getTokenFactory();
		}
	}
}
