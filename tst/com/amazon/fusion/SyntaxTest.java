// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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
        expectSyntaxFailure("(quote_syntax)");
        expectSyntaxFailure("(quote_syntax foo bar)");
    }


    @Test
    public void syntaxAppendArity()
        throws Exception
    {
        expectArityFailure("(syntax_append)");
    }


    @Test
    public void syntaxSubseqArity()
        throws Exception
    {
        expectArityFailure("(syntax_subseq)");
        expectArityFailure("(syntax_subseq (quasisyntax ()))");
        expectArityFailure("(syntax_subseq (quasisyntax ()) 1 2)");
    }

    @Test
    public void syntaxSubseqIndexType()
        throws Exception
    {
        expectContractFailure("(syntax_subseq (quasisyntax ()) null)");
    }


    @Test
    public void quasisyntaxArity()
        throws Exception
    {
        expectSyntaxFailure("(quasisyntax)");
        expectSyntaxFailure("(quasisyntax foo bar)");

        expectSyntaxFailure("(quasisyntax (a (quasisyntax)))");
        expectSyntaxFailure("(quasisyntax (a (quasisyntax foo bar)))");

        expectSyntaxFailure("(quasisyntax (a (unsyntax)))");
        expectSyntaxFailure("(quasisyntax (a (unsyntax foo bar)))");

        // deeper unsyntax
        expectSyntaxFailure("(quasisyntax (a (quasisyntax (b (unsyntax)))))");
        expectSyntaxFailure("(quasisyntax (a (quasisyntax (b (unsyntax foo bar)))))");
    }

    @Test
    public void tooManyUnsyntaxes()
        throws Exception
    {
        expectSyntaxFailure("(quasisyntax (unsyntax (unsyntax bar)))");
    }

    @Test
    public void unsyntaxValueNotSyntax()
        throws Exception
    {
        expectContractFailure("(quasisyntax (unsyntax 12))");
    }

    @Test
    public void unsyntaxOutOfQuasisyntax()
        throws Exception
    {
        expectSyntaxFailure("(unsyntax)");
        expectSyntaxFailure("(unsyntax foo)");
    }

    @Test
    public void transformerReturnsNonSyntax()
        throws Exception
    {
        eval("(define_syntax S (lambda (stx) 99))");
        expectSyntaxFailure("(S)");
    }
}
