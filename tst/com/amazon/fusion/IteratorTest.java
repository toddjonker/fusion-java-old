// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIterator.injectIonIterator;
import static com.amazon.fusion.FusionIterator.injectIterator;
import com.amazon.ion.IonList;
import java.util.ArrayList;
import org.junit.Test;


public class IteratorTest
    extends CoreTestCase
{
    @Test()
    public void testIterateValueFailures()
        throws Exception
    {
        expectArityFailure("(value_iterator)");
        expectArityFailure("(value_iterator 1 2)");
    }


    @Test
    public void testIteratorAppendFailures()
        throws Exception
    {
        expectArityFailure("(iterator_append)");
        expectArityFailure("(iterator_append empty_iterator)");
        expectArityFailure("(iterator_append empty_iterator empty_iterator empty_iterator)");

        expectContractFailure("(iterator_append empty_iterator [])");
        expectContractFailure("(iterator_append [] empty_iterator)");
        expectContractFailure("(iterator_append empty_iterator null)");
        expectContractFailure("(iterator_append null empty_iterator)");
    }


    @Test
    public void testIteratorFilterFailures()
        throws Exception
    {
        expectArityFailure("(iterator_choose)");
        expectArityFailure("(iterator_choose is_null)");
        expectArityFailure("(iterator_choose is_null empty_iterator 1)");

        // TODO FUSION-85 need to check type of the proc
//      expectContractFailure("(iterator_choose 1 empty_iterator)");
        expectContractFailure("(iterator_choose is_null [])");
    }


    @Test
    public void testIteratorMapFailures()
        throws Exception
    {
        expectArityFailure("(iterator_map +)");
        expectArityFailure("(iterator_map + empty_iterator empty_iterator)");

        eval("(define plus1 (lambda (n) (+ 1 n)))");

        // TODO FUSION-85 need to check type of the proc
//      expectContractFailure("(iterator_map 1 empty_iterator)");
        expectContractFailure("(iterator_map plus1 [])");
    }


    @Test
    public void testIteratorMapSplicingFailures()
        throws Exception
    {
        expectArityFailure("(iterator_map_splicing value_iterator)");
        expectArityFailure("(iterator_map_splicing value_iterator empty_iterator empty_iterator)");

        // TODO FUSION-85 need to check type of the proc
//      expectContractFailure("(iterator_map_splicing 1 empty_iterator)");
        expectContractFailure("(iterator_map_splicing value_iterator [])");
    }


    //========================================================================
    // Injection APIs

    private static final String ITER_HAS_NEXT = "(iterator_has_next iter)";
    private static final String ITER_NEXT = "(iterator_next iter)";

    @Test
    public void testIteratorInjecting()
        throws Exception
    {
        ArrayList<Object> list = new ArrayList<Object>();
        list.add("str");
        list.add(12);
        list.add(null);

        TopLevel top = topLevel();
        top.define("iter", injectIterator(null, list.iterator()));
        assertString("str", ITER_NEXT);
        assertEval(12, ITER_NEXT);
        assertVoid(ITER_NEXT);
        assertEval(false, ITER_HAS_NEXT);
    }

    @Test
    public void testIonIteratorInjecting()
        throws Exception
    {
        IonList list = (IonList) system().singleValue("['''str''', 12, null]");

        TopLevel top = topLevel();
        top.define("iter", injectIterator(null, list.iterator()));
        assertEval(true, ITER_HAS_NEXT);
        assertString("str", ITER_NEXT);
        assertEval(12, ITER_NEXT);
        assertEval("null.null", ITER_NEXT);
        assertEval(false, ITER_HAS_NEXT);

        top.define("iter", injectIonIterator(null, list.iterator()));
        assertEval(true, ITER_HAS_NEXT);
        assertString("str", ITER_NEXT);
        assertEval(12, ITER_NEXT);
        assertEval("null.null", ITER_NEXT);
        assertEval(false, ITER_HAS_NEXT);
    }
}
