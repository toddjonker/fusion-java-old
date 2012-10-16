// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Before;
import org.junit.Test;

/**
 * Test cases for the function ion module
 */
public class FunctionalOperatorTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws Exception
    {
        topLevel().requireModule("/fusion/function");
        topLevel().requireModule("/fusion/stream");
    }

    @Test
    public void testCompose()
        throws Exception
    {
        // usr + lib
        eval("(define is_zero (lambda (x) (= x 0)))");
        eval("(define numCheck (compose is_null is_zero))");
        assertEval(false, "(numCheck 0)");

        // usr + usr - int -> int / recursive (somewhat)
        eval("(define add1 (lambda (x) (+ x 1)))");
        eval("(define add2 (compose add1 add1))");
        eval("(define add3 (compose add1 add2))");
        assertEval(6, "(add3 3)");

        // streams - inject + stream_to_list
        eval("(define intToList (compose stream_to_list inject_stream))");
        assertEval("[2]","(intToList 2)");
    }

    @Test
    public void testPredicateAnd()
        throws Exception
    {
        eval("(define is_zero (lambda (x) (= x 0)))");
        eval("(define invalidNumber (predicate_and is_zero is_null))");
        //assertEval(false,"(invalidNumber 0)");
        //assertEval(false, "(invalidNumber 1)");

        // ensures that the second code does not get exec'd
        eval("(define assertFail (lambda (x) (assert false \"doi\")))");
        eval("(define testAnd (predicate_and is_zero assertFail))");
        assertEval(false, "(testAnd 1)");
    }

    @Test
    public void testPredicateOr()
        throws Exception
    {
        eval("(define is_zero (lambda (x) (= x 0)))");
        eval("(define is_one (lambda (x) (= x 1)))");
        eval("(define invalidNumber (predicate_or is_zero is_one))");
        //assertEval(true, "(invalidNumber 0)");
        //assertEval(false, "(invalidNumber 2)");

        // ensures that the second code does not get exec'd
        eval("(define assertFail (lambda (x) (assert false \"doi\")))");
        eval("(define testOr (predicate_or is_zero assertFail))");
        assertEval(true, "(testOr 0)");
    }

    @Test
    public void testNegate()
        throws Exception
    {
        // negated lib fxn
        eval("(define is_not_null (negate is_null))");
        assertEval(true, "(is_not_null 1)");
        assertEval(false, "(is_not_null null)");

        // negated user fxn
        eval("(define is_not_stream (negate is_stream))");
        assertEval(true, "(is_not_stream 1)");
        assertEval(false, "(is_not_stream (stream_for [1]))");
        assertEval(false, "(is_not_stream empty_stream)");
    }
}
