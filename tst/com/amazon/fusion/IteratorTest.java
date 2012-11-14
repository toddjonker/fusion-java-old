// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Before;
import org.junit.Test;


public class IteratorTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws Exception
    {
        topLevel().requireModule("/fusion/iterator");
    }


    @Test()
    public void testIterateValueFailures()
        throws Exception
    {
        expectArityFailure("(iterate_value)");
        expectArityFailure("(iterate_value 1 2)");
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
        expectArityFailure("(iterator_filter)");
        expectArityFailure("(iterator_filter is_null)");
        expectArityFailure("(iterator_filter is_null empty_iterator 1)");

        // TODO FUSION-85 need to check type of the proc
//      expectContractFailure("(iterator_filter 1 empty_iterator)");
        expectContractFailure("(iterator_filter is_null [])");
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
        expectArityFailure("(iterator_map_splicing iterate_value)");
        expectArityFailure("(iterator_map_splicing iterate_value empty_iterator empty_iterator)");

        // TODO FUSION-85 need to check type of the proc
//      expectContractFailure("(iterator_map_splicing 1 empty_iterator)");
        expectContractFailure("(iterator_map_splicing iterate_value [])");
    }
}
