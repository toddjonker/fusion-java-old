// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Before;
import org.junit.Test;

/**
 * See ftst/parameter.test.ion
 */
public class ParameterTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws Exception
    {
        topLevel().requireModule("/fusion/parameter");
    }

    @Test
    public void testParameterArity()
        throws Exception
    {
        expectArityExn("(make_parameter)");
        expectArityExn("(make_parameter 1 2)");
    }

    @Test
    public void testParameterizeSyntax()
        throws Exception
    {
        eval("(define p (make_parameter 1))");

        expectSyntaxExn("(parameterize)");
        expectSyntaxExn("(parameterize ((p 1)))");
        expectSyntaxExn("(parameterize 12 13)");
        expectSyntaxExn("(parameterize null.sexp 13)");
        expectSyntaxExn("(parameterize (12) 13)");
        expectSyntaxExn("(parameterize (1 2) 13)");
        expectSyntaxExn("(parameterize (()) 13)");
        expectSyntaxExn("(parameterize ((12)) 13)");
        expectSyntaxExn("(parameterize ((p)) 13)");
        expectSyntaxExn("(parameterize ((p 1 2)) 13)");
        expectSyntaxExn("(parameterize ((p 1) ()) 13)");
        expectSyntaxExn("(parameterize ((p 1) (p2)) 13)");

        expectContractExn("(parameterize ((1 2)) 13)");
    }
}
