// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Before;
import org.junit.Test;

public class SyntaxTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws Exception
    {
        topLevel().requireModule("/fusion/experimental/syntax");
    }


    @Test
    public void quoteSyntaxArity()
        throws Exception
    {
        expectSyntaxExn("(quote_syntax)");
        expectSyntaxExn("(quote_syntax foo bar)");
    }


    @Test
    public void syntaxAppendArity()
        throws Exception
    {
        expectArityExn("(syntax_append)");
    }


    @Test
    public void syntaxSubseqArity()
        throws Exception
    {
        expectArityExn("(syntax_subseq)");
        expectArityExn("(syntax_subseq (quasisyntax ()))");
        expectArityExn("(syntax_subseq (quasisyntax ()) 1 2)");
    }

    @Test
    public void syntaxSubseqIndexType()
        throws Exception
    {
        expectContractExn("(syntax_subseq (quasisyntax ()) null)");
    }


    @Test
    public void quasisyntaxArity()
        throws Exception
    {
        expectSyntaxExn("(quasisyntax)");
        expectSyntaxExn("(quasisyntax foo bar)");

        expectSyntaxExn("(quasisyntax (a (quasisyntax)))");
        expectSyntaxExn("(quasisyntax (a (quasisyntax foo bar)))");

        expectSyntaxExn("(quasisyntax (a (unsyntax)))");
        expectSyntaxExn("(quasisyntax (a (unsyntax foo bar)))");

        // deeper unsyntax
        expectSyntaxExn("(quasisyntax (a (quasisyntax (b (unsyntax)))))");
        expectSyntaxExn("(quasisyntax (a (quasisyntax (b (unsyntax foo bar)))))");
    }

    @Test
    public void tooManyUnsyntaxes()
        throws Exception
    {
        expectSyntaxExn("(quasisyntax (unsyntax (unsyntax bar)))");
    }

    @Test
    public void unsyntaxValueNotSyntax()
        throws Exception
    {
        expectContractExn("(quasisyntax (unsyntax 12))");
    }

    @Test
    public void unsyntaxOutOfQuasisyntax()
        throws Exception
    {
        expectSyntaxExn("(unsyntax)");
        expectSyntaxExn("(unsyntax foo)");
    }

    @Test
    public void transformerReturnsNonSyntax()
        throws Exception
    {
        eval("(define_syntax S (lambda (stx) 99))");
        expectSyntaxExn("(S)");
    }
}
