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

    @Test
    public void testList()
    {
        eval("(define x 13)");
        assertEval("[13]", "[x]");
        assertEval("[12,13,14]", "[12,x,14]");
        assertEval("[12,[[13]],14]", "[12,[[x]],14]");
        eval("(define y [x])");
        assertEval("[13]", "y");
        assertEval("[1,[13]]", "[ 1, y]");
        eval("(define x 23)");
        assertEval("[1,{f:[13]}]", "[1, {f:y}]");
        eval("(define z (func () [x]))");
        assertEval("[23]", "(z)");
        eval("(define x 33)");
        assertEval("[33]", "(z)");
    }

    @Test
    public void testStruct()
    {
        eval("(define x 13)");
        assertEval("{f:13}", "{f:x}");
        assertEval("{e:12,f:13,g:14}", "{g:14,f:x,e:12}");
        assertEval("{e:12,f:[[13]],g:14}", "{e:12,f:[[x]],g:14}");
        eval("(define y {f:x})");
        assertEval("{f:13}", "y");
        assertEval("{f:1,f:{f:13}}", "{ f:1, f:y}");
        eval("(define x 23)");
        assertEval("{g:1,f:[{f:13}]}", "{ g:1, f:[y]}");
        eval("(define z (func () {a:{b:x}}))");
        assertEval("{a:{b:23}}", "(z)");
        eval("(define x 33)");
        assertEval("{a:{b:33}}", "(z)");
    }
}
