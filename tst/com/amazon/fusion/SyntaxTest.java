// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Before;
import org.junit.Test;

public class SyntaxTest
    extends CoreTestCase
{
    @Before
    public void uses()
        throws Exception
    {
        eval("(use 'fusion/syntax')");
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
        expectArityFailure("(syntax_append (quasisyntax 1))");
        expectArityFailure("(syntax_append (quasisyntax 1)(quasisyntax 2)(quasisyntax 3))");
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


}
