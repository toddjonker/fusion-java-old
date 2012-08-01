// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Before;
import org.junit.Test;

/**
 * Contains test cases for the stream classes and operations
 */
public class StringTest
    extends CoreTestCase
{

    @Before
    public void setupTest()
        throws FusionException
    {
        eval("(use 'fusion/string')");
    }

    @Test
    public void testStringConcat()
        throws Exception
    {
        assertString("", "(concatenate)");
        assertString("hello","(concatenate \"hello\")");
        assertString("hello world", "(concatenate \"hello \" \"world\")");
        assertString("I like pie", "(concatenate \"I \" \"like \" \"pie\")");
    }

    @Test
    public void testStringConcatInvalid()
        throws Exception
    {
        expectArgTypeFailure("(concatenate 1)",0);
        expectArgTypeFailure("(concatenate true)",0);
    }

    @Test
    public void testStringCaseTransform()
        throws Exception
    {
        String [] ans = { "a", "A" };
        String [] ops = { "string_to_lower", "string_to_upper" };

        for (int i = 0; i < ops.length; i++)
        {
            assertString(ans[i],"("+ops[i]+" \"A\")");
            assertString(ans[i],"("+ops[i]+"\"a\")");
            assertString("", "("+ops[i]+ "\"\")");
        }
    }

    @Test
    public void testStringCaseTransformFail()
        throws Exception
    {
        String [] ops = { "string_to_upper", "string_to_lower" };

        for (String op : ops)
        {
            expectArityFailure("("+op+")");
            expectArityFailure("("+op+" \"a\" \"b\")");

            expectContractFailure("("+op+" {})");
            expectContractFailure("("+op+" null.string)");
        }

    }

}
