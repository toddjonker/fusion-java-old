// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 *
 */
public class ListTest
    extends CoreTestCase
{
    @Before
    public void listTestBegin()
        throws Exception
    {
        eval("(use 'fusion/list')");

        eval("(define smList "+ionListGenerator(1)+")");
        eval("(define medList "+ionListGenerator(3)+")");
        eval("(define lgList "+ionListGenerator(8888)+")");
    }

    @Test
    public void testAdd()
        throws Exception
    {
        assertEval("[1]", "(add null.list 1)");
        assertEval("[1]", "(add [] 1)");

        assertEval("(1)", "(add (quote null.sexp) 1)");
        assertEval("(1)", "(add (quote ()) 1)");
    }

    @Test
    public void testAddAnyIon()
        throws Exception
    {
        for (String form : allIonExpressions())
        {
            String expr = "(add [] " + form + ")";
            IonList result = (IonList) evalToIon(expr);
            Assert.assertEquals(1, result.size());
        }
    }

    @Test
    public void testDeepAdd()
        throws Exception
    {
        assertEval("{f:[2]}",
                   "(let ((s {f:[]}))" +
                   "  (add (. s \"f\") 2)" +
                   "  s)");

        assertEval("{f:(2)}",
                   "(let ((s {f:(quote ())}))" +
                   "  (add (. s \"f\") 2)" +
                   "  s)");
    }

    @Test
    public void testAddArity()
        throws Exception
    {
        expectArityFailure("(add)");
    }


    @Test
    public void testAddArgType()
        throws Exception
    {
        for (String form : nonSequenceExpressions())
        {
            String expr = "(add " + form + " 12)";
            expectArgTypeFailure(expr, 0);
        }

        for (String form : nonIonExpressions())
        {
            String expr = "(add [] " + form + ")";
            expectArgTypeFailure(expr, 1);
        }
    }


    //========================================================================


    @Test
    public void testSize()
        throws Exception
    {
        assertEval(0, "(size null.list)");
        assertEval(0, "(size [])");
        assertEval(1, "(size [1])");
        assertEval(2, "(size [2,2])");

        assertEval(0, "(size (quote null.sexp))");
        assertEval(0, "(size (quote ()))");
        assertEval(1, "(size (quote (1)))");
        assertEval(2, "(size (quote (2 2)))");
    }


    @Test
    public void testSizeArity()
        throws Exception
    {
        expectArityFailure("(size)");
        expectArityFailure("(size [] [])");
    }

    @Test
    public void testSizeArgType()
        throws Exception
    {
        for (String form : nonContainerExpressions())
        {
            String expr = "(size " + form + ")";
            expectArgTypeFailure(expr, 0);
        }
    }

    public String ionListGeneratorWithOffset(int length, int offset)
    {
        StringBuilder resultStr = new StringBuilder("[");

        for (int i = 0; i < length; i++)
        {
            resultStr.append(Integer.toString(i+offset));
            if (i != length-1)
            {
                resultStr.append(",");
            }

        }

        resultStr.append("]");

        return resultStr.toString();
    }

    public String ionListGenerator(int length)
    {
        return ionListGeneratorWithOffset(length, 0);
    }

    @Test
    public void testRest()
        throws Exception
    {
        assertEval("[0]","(rest smList 0)");
        assertEval("[1,2]","(rest medList 1)");
        assertEval(ionListGeneratorWithOffset(8887,1), "(rest lgList 1)");

        assertUndef("(rest [] 1)");
        assertUndef("(rest lgList 10000)");
    }

    @Test
    public void testRestFail()
        throws Exception
    {
        expectArityFailure("(rest)");
        expectArityFailure("(rest 1 2 3 4)");
        expectArityFailure("(rest [])");

        expectArgTypeFailure("(rest \"hello\" 0)", 0);
    }

    @Test
    public void testSubsequence()
        throws Exception
    {
        assertEval("[0]", "(subseq smList 0 0)");
        assertEval("[1,2]", "(subseq medList 1 2)");
        assertEval(ionListGeneratorWithOffset(1000,4000),
                "(subseq lgList 4000 4999)");

        // TODO design question: undef to signal error -
        // but don't want to crash pgrm
        assertUndef("(subseq lgList 8886 6666)");
        assertUndef("(subseq smList 5 8)");
        assertUndef("(subseq lgList 5555 88888)");
    }

    @Test
    public void testSubsequenceFail()
        throws Exception
    {
        expectArityFailure("(subseq)");
        expectArityFailure("(subseq 1 2 3 4)");
        expectArityFailure("(subseq [])");

        expectArgTypeFailure("(subseq \"optimus prime\" 2 3)", 0);
    }

    @Test
    public void testLast()
        throws Exception
    {
        assertEval(0,"(last smList)");
        assertEval(2,"(last medList)");
        assertEval(8887,"(last lgList)");

        assertUndef("(last [])");
    }

    @Test
    public void testLastFail()
        throws Exception
    {
        expectArityFailure("(last)");
        expectArityFailure("(last [] 2)");

        expectArgTypeFailure("(last \"pikachu\")", 0);
    }

    @Test
    public void testFirst()
        throws Exception
    {
        assertEval(0,"(first smList)");
        assertEval(0,"(first medList)");
        assertEval(4000,"(first "+ionListGeneratorWithOffset(1000,4000)+")");

        assertUndef("(first [])");
    }

    @Test
    public void testFirstFail()
        throws Exception
    {
        expectArityFailure("(first)");
        expectArityFailure("(first [] 2)");

        expectArgTypeFailure("(first \"boeing\")", 0);

    }

    @Test
    public void testListIntersect()
        throws Exception
    {
        assertEval("[3]", "(list_intersect [3,5] [3,4])");
        assertEval("[3]", "(list_intersect [5,3] [3,4])");
        assertEval("[]",  "(list_intersect [[[]]] [[]])");
    }

    @Test
    public void testListIntersectFail()
        throws Exception
    {
        expectArgTypeFailure("(list_intersect {} [])",0);

        expectArityFailure("(list_intersect)");
        expectArityFailure("(list_intersect [])");
    }

    public void listContains()
        throws Exception
    {
        String smallList = "[1,2,3]";

        assertEval(true,  "(contains "+smallList+" 2)");
        assertEval(false, "(contains "+smallList+" 4)");
        assertEval(false, "(contains "+smallList+" "+smallList+")");
        assertEval(true,  "(contains ["+smallList+"] "+smallList+")");
        assertEval(true,  "(contains ["+smallList+",{A:3}] {A:3})");
        assertEval(true,  "(contains [{A:3, B:2}, {A:3, B:1}] {A:3, B:1})");
        assertEval(false, "(contains [{A:3, B:2}, {A:3, B:1}] {A:3, B:4})");
        assertEval(false, "(contains [] null.int)");
        assertEval(false, "(contains [] 5)");
    }

    @Test
    public void listContainsFail()
        throws Exception
    {
        expectArityFailure("(contains)");
        expectArityFailure("(contains [] [] [])");

        expectContractFailure("(contains 1 1)");
        expectContractFailure("(contains \"hello\" \"hello\")");
    }

    @Test
    public void testIsList()
        throws Exception
    {
        assertEval(true, "(is_list [])");
        assertEval(true, "(is_list [1,3])");
        assertEval(true, "(is_list [[[]],[3,3]])");
        assertEval(true, "(is_list [{a:3},{a:3}])");

        assertEval(false, "(is_list undef)");
        assertEval(false, "(is_list {})");
        assertEval(false, "(is_list \"[1,2,3]\")");
    }
}
