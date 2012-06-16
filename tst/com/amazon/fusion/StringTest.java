// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 * Contains test cases for the stream classes and operations
 */
public class StringTest
    extends CoreTestCase
{

    @Test
    public void testStringConcat()
        throws Exception
    {
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

}
