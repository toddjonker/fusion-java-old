// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;


public class LiteralEvalTest
    extends CoreTestCase
{
    @Test
    public void testBooleans()
    {
        assertSelfEval("null.bool");
        assertSelfEval("true");
        assertSelfEval("false");
    }
    
    @Test
    public void testIntegers()
    {
        assertSelfEval("null.int");
        assertSelfEval("-98763");
        assertSelfEval("0");
        assertSelfEval("12");
        assertSelfEval("12");
        assertSelfEval("10");
    }
    
    @Test
    public void testNull()
    {
        assertSelfEval("null");
        assertEval("null", "null.null");
    }
    
    @Test
    public void testTimestamp()
    {
        assertSelfEval("null.timestamp");
        assertSelfEval("2012T");
        assertSelfEval("null.timestamp");
        assertSelfEval("2012-03T");
        assertSelfEval("null.timestamp");
        assertSelfEval("2012-03-21T");
        assertSelfEval("2012-03-21T02:57-07:00");
    }
}
