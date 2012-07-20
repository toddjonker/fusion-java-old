// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.printString;

import java.io.File;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Contains test cases for the stream classes and operations
 */
public class StreamTest
    extends ExternalTest
{
    @Before
    public void setUp()
        throws Exception
    {
        eval("(use 'fusion/stream')");
    }


    @Test
    public void testValidConversions()
        throws Exception
    {
        assertEval("[]","(stream_to_list (stream_for []) )");
        assertEval("[1]","(stream_to_list (stream_for [1]) )");
        assertEval("[\"hello\", \"world\"]",
                   "(stream_to_list " +
                   " (stream_for [\"hello\", \"world\"]) )");
        assertEval("[[1,2,3],[4,5,6],[7,8,9]]",
                   "(stream_to_list " +
                   " (stream_for [[1,2,3],[4,5,6],[7,8,9]]) )");
    }

    @Test
    public void testInvalidConversions()
        throws Exception
    {
        expectContractFailure("(stream_for 3)");
        expectContractFailure("(stream_to_list 3)");
        expectContractFailure("(stream_to_list [2,3])");
    }

    @Test
    public void testEmptyStream()
        throws Exception
    {
        assertEval("[]","(stream_to_list empty_stream)");
        assertEval(false,"(is_null (stream_has_next empty_stream))");
    }

    @Test
    public void testInjectStream()
        throws Exception
    {
        assertEval("[[]]","(stream_to_list (inject_stream []) )");
        assertEval("[1]","(stream_to_list (inject_stream 1) )");
        assertEval("[\"hello\"]",
                   "(stream_to_list (inject_stream \"hello\") )");
        assertEval("[[\"hello\", \"world\"]]",
                   "(stream_to_list " +
                   " (inject_stream [\"hello\", \"world\"]) )");
        assertEval("[[[1,2,3,4],[3.3,2.5,5.6],[\"hello\"]]]",
                   "(stream_to_list" +
                   " (inject_stream [[1,2,3,4],[3.3,2.5,5.6],[\"hello\"]]) )");
        assertEval("[7]", "(stream_to_list (inject_stream (+ 1 2 4)) )");

        // TODO support streams within stream for generality
        // inter-class testing -- test case fails but is valid, but feature
        // not supported yet
        //assertEval("[]", "(stream_to_list(inject_stream (empty_stream)))");
    }

    @Test()
    public void testStreamClassArityCheck()
        throws Exception
    {
        expectArityFailure("(stream_to_list)");
        expectArityFailure("(stream_for)");
        expectArityFailure("(inject_stream)");
        expectArityFailure("(stream_union [1,2,3])");
        expectArityFailure("(stream_project [1,2,3])");
    }

    // Utility test functions
    @Test
    public void testNext()
        throws Exception
    {
        // TODO Streams within streams
        //assertEval("true","(is_null (stream_next (empty_stream)))");
        // <-- causes NoSuchElementException
        assertEval("5", "(stream_next(stream_for [5]))");
    }

    @Test
    public void testStreamIteration()
        throws Exception
    {
        eval("(define s (stream_for [1,2,3,4]))");
        assertEval(1, "(stream_next s)");
        assertEval(2, "(stream_next s)");
        assertEval(3, "(stream_next s)");
        assertEval(true, "(stream_has_next s)");
        assertEval(4, "(stream_next s)");
        assertEval(false, "(stream_has_next s)");
    }

    @Test
    public void testHasNext()
        throws Exception
    {
        assertEval(false,"(stream_has_next empty_stream)");
        assertEval(true, "(stream_has_next (stream_for [8]))");
    }

    @Test
    public void testUnion()
        throws Exception
    {
        assertEval("[]",
                   "(stream_to_list" +
                   " (stream_union" +
                   "  empty_stream" +
                   "   empty_stream) )");
        assertEval("[1,2]",
                   "(stream_to_list" +
                   " (stream_union" +
                   "  empty_stream"+
                   "   (stream_for [1,2]) ) )");
        assertEval("[1,2,3,4]",
                   "(stream_to_list" +
                   " (stream_union" +
                   "  (stream_for [1,2])" +
                   "   (stream_for [3,4]) ) )");
        assertEval("[[1,2],[],5]",
                   "(stream_to_list" +
                   " (stream_union" +
                   "  (stream_for [[1,2]])" +
                   "   (stream_for [[],5]) ) )");

        // TODO stream within stream
        // assertEval("[[],[],[]]","(stream_to_list(stream_union (stream_for
        // [(empty_stream),[]]) (stream_for [])))");

        // TODO remove duplicates in stream union
        // assertEval("[1,2]", "(stream_to_list(stream_union (stream_for [1,2])
        // (stream_for [1,2])))");

        // TODO union several streams together
        // assertEval("[1,2,3]", "(stream_to_list(stream_union (stream_for [1])
        // (stream_for [1,2]) (stream_for [1,2,3])))");
    }

    @Test
    public void testInvalidUnion()
        throws Exception
    {
        expectContractFailure("(stream_union [1,2,3] [])");
        // TODO invalid TCs
        // expectContractFailure("(stream_union [1,2,3] s)");
        // expectContractFailure("(stream_union (stream_for [1,2,3]) s)");
    }

    @Test
    public void testProject()
        throws Exception
    {

        // test lib fxn transformation from <list> -> bool
        assertEval("[false,false]",
                   "(stream_to_list" +
                   " (stream_project " +
                   "  (stream_for [1,2]) is_null))");

        // test usr-defn'd fxn
        eval("(define add2 (lambda (x) (+ x 2)))");
        assertEval("[7,9,11,13]",
                   "(stream_to_list" +
                   " (stream_project " +
                   "  (stream_for [5,7,9,11]) add2))");
    }

    @Test
    public void testInvalidProjections()
        throws Exception
    {
        expectContractFailure("(stream_project [1,2,3] 1)");
        // TODO invalid TC
        // expectContractFailure("(stream_project [1,2,3] p)");
        // expectContractFailure("(stream_project (stream_for [1,2,3]) p)");
    }


    @Test
    public void testSelect()
        throws Exception
    {
        eval("(define is_zero (lambda (x) (= x 0)))");
        assertEval("[0]",
                   "(stream_to_list" +
                   " (stream_select" +
                   "  (stream_for [0,1]) is_zero))");
        eval("(define is_one (lambda (x) (= x 1)))");
        eval("(define is_odd" +
             " (lambda (x) " +
             "  (if (is_one x) true " +
             "    (if (is_zero x) false" +
             "     (is_odd (- x 2))))))");
        assertEval("[1,3,5,7,9]",
                   "(stream_to_list" +
                   " (stream_select" +
                   "(stream_for [1,2,3,4,5,6,7,8,9]) is_odd))");

        eval("(define rightSize (lambda (x) (= 4 (size x))))");
        assertEval("[[1,2,3,4]]",
                   "(stream_to_list" +
                   " (stream_select" +
                   "  (stream_for" +
                   "   [[1,2,3],[1,2,3,4],[1,2,3,4,5],[]]) rightSize))");
    }

    @Test
    public void testInvalidSelects()
        throws Exception
    {
        expectContractFailure("(stream_select [1,2,3] 1)");

        // TODO invalid test case
        //expectContractFailure("(stream_select [1,2,3] p)");

        // TODO invalid test case
        // eval("(define is_one (lambda (x) (= x 1)))");
        //expectFusionException("(stream_select (stream_for [[1,2,3]," +
        //                      "1,[2,3],[]]) is_one)");
    }

    @Test
    public void testCrossApply()
        throws Exception
    {

        // User-defined - one-to-one mapping / single elt return
        // TODO invalid TC -- should return a stream -- stream_inject(...)
        eval("(define add1 (lambda (x) (+ x 1)))");
        assertEval("[]",
                   "(stream_to_list" +
                   " (stream_cross_apply " +
                   "  (stream_for []) add1))");
        //assertEval("[1]",
        //           "(stream_to_list" +
        //           " (stream_cross_apply " +
        //           "  (stream_for [0]) add1))");

        // User-defined - one-to-many mapping / list return
        eval("(define seqExpandList" +
             " (lambda (x)" +
             "  (add" +
             "   (add " +
             "    (add " +
             "     (add" +
             "      (add [] (+ x 1)) (+ x 2)) (+ x 3)) (+ x 4)) (+ x 5))))");
        assertEval("[1,2,3,4,5]",
                   "(stream_to_list " +
                   " (stream_cross_apply" +
                   "  (stream_for [0]) seqExpandList))");

        // User-defined - many-to-many mapping / stream return
        eval("(define seqExpand " +
             "(lambda (x)" +
             " (stream_for" +
             "  (add (add (add (add (add []" +
             "   (+ x 1)) (+ x 2)) (+ x 3)) (+ x 4)) (+ x 5)))))");
        assertEval("[1,2,3,4,5,2,3,4,5,6,3,4,5,6,7]",
                   "(stream_to_list" +
                   " (stream_cross_apply" +
                   "  (stream_for [0,1,2])" +
                   "   seqExpand))");

        // Lib fxn
        //assertEval("[false,false,false,true,false]",
        //           "(stream_to_list" +
        //           " (stream_cross_apply" +
        //           "  (stream_for" +
        //           "   [[],0,[1,2],null,[null]])" +
         //          "    is_null))");
    }

    @Test
    public void testCrossProduct()
        throws Exception
    {
         assertEval("[]",
                    "(stream_to_list" +
                    " (stream_cross_product" +
                    "  (stream_for [])" +
                    "   (stream_for [3,4])))");
         assertEval("[[1,3],[1,4],[2,3],[2,4]]",
                    "(stream_to_list" +
                    " (stream_cross_product" +
                    "  (stream_for [1,2])" +
                    "   (stream_for [3,4])))");
    }

    @Test
    public void testStreamForFile()
        throws Exception
    {
        File f = getProjectFile("tst/com/amazon/fusion/ints.ion");
        String path = f.getAbsolutePath();
        eval("(define s (stream_for_file " + printString(path) + "))");
        assertEval(0, "(stream_next s)");
        assertEval(true, "(stream_has_next s)");
        assertEval(1, "(stream_next s)");
        assertEval(2, "(stream_next s)");
        assertEval(3, "(stream_next s)");
        assertEval(false, "(stream_has_next s)");
    }

    @Test
    public void testIsStream()
        throws Exception
    {
         assertEval(false,
                    "(is_stream 0)");
         assertEval(true,
                    "(is_stream empty_stream)");
         assertEval(false,
                    "(is_stream [0,1,2])");
         assertEval(false,
                    "(is_stream \"hello there\")");
         assertEval(true,
                    "(is_stream (stream_for [0,1,2]))");
         assertEval(true,
                    "(is_stream (stream_for [[[]],null,\"hello\",[0,1,2]]))");
    }

    @Test @Ignore
    public void testForListAsStream()
        throws Exception
    {
        assertEval("[]","(for_list((s (empty_stream))) s)");
        //assertEval("[2,3,4,5]","(list_to_stream(for_list (stream_for [1,2,3,4]) (+ x 1)))");
    }

}
