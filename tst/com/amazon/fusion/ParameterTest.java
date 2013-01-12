// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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
        expectArityFailure("(make_parameter)");
        expectArityFailure("(make_parameter 1 2)");
    }

    @Test
    public void testParameterizeSyntax()
        throws Exception
    {
        eval("(define p (make_parameter 1))");

        expectSyntaxFailure("(parameterize)");
        expectSyntaxFailure("(parameterize ((p 1)))");
        expectSyntaxFailure("(parameterize 12 13)");
        expectSyntaxFailure("(parameterize null.sexp 13)");
        expectSyntaxFailure("(parameterize (12) 13)");
        expectSyntaxFailure("(parameterize (1 2) 13)");
        expectSyntaxFailure("(parameterize (()) 13)");
        expectSyntaxFailure("(parameterize ((12)) 13)");
        expectSyntaxFailure("(parameterize ((p)) 13)");
        expectSyntaxFailure("(parameterize ((p 1 2)) 13)");
        expectSyntaxFailure("(parameterize ((p 1) ()) 13)");
        expectSyntaxFailure("(parameterize ((p 1) (p2)) 13)");

        expectContractFailure("(parameterize ((1 2)) 13)");
    }
}
