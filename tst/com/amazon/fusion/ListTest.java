// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.unsafeVectorRef;
import static com.amazon.fusion.FusionVector.unsafeVectorSize;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
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
        topLevel().requireModule("/fusion/iterator");
        topLevel().requireModule("/fusion/list");
        topLevel().requireModule("/fusion/vector");

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
    public void testMutationOfConstants()
        throws Exception
    {
        eval("(define nl (lambda () null.list))");
        assertEval("[1]", "(add (nl) 1)");
        assertEval("[2]", "(add (nl) 2)");

        eval("(define l (lambda () [1]))");
        assertEval("[1, 2]", "(add (l) 2)");
        assertEval("[1, 3]", "(add (l) 3)");
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
    public void testSubsequence()
        throws Exception
    {
        assertEval("[]",    "(subseq [] 0 0)");
        assertEval("[]",    "(subseq smList 0 0)");
        assertEval("[0]",   "(subseq smList 0 1)");
        assertEval("[1]",   "(subseq medList 1 2)");
        assertEval("[1,2]", "(subseq medList 1 3)");
        assertEval(ionListGeneratorWithOffset(1000,4000),
                "(subseq lgList 4000 5000)");

        // I'm not certain how to specify the result in this case.
        // Should it return null.list or [] ?
        eval("(subseq null.list 0 0)");
    }

    @Test
    public void testSubsequenceFail()
        throws Exception
    {
        expectArityFailure("(subseq)");
        expectArityFailure("(subseq 1 2 3 4)");
        expectArityFailure("(subseq [])");

        expectArgTypeFailure("(subseq \"optimus prime\" 2 3)", 0);

        // Problem here is index too large, not null.list
        expectArgTypeFailure("(subseq null.list 0 1)", 2);

        expectArgTypeFailure("(subseq lgList 8886 6666)", 1);
        expectArgTypeFailure("(subseq smList 5 8)", 2);
        expectArgTypeFailure("(subseq lgList 5555 88888)", 2);

        // Test conversion of long to int
        long big = 4294967298L;
        assertEquals(2, (int)big);
        expectArgTypeFailure("(subseq smList " + big + " " + (big + 1) + ")", 1);

        big = -9223372036854775806L;
        assertEquals(2, (int)big);
        expectArgTypeFailure("(subseq smList " + big + " " + (big + 1) + ")", 1);
    }


    @Test
    public void testLast()
        throws Exception
    {
        assertEval(0,"(last smList)");
        assertEval(2,"(last medList)");
        assertEval(8887,"(last lgList)");
    }

    @Test
    public void testLastFail()
        throws Exception
    {
        expectArityFailure("(last)");
        expectArityFailure("(last [] 2)");

        expectArgTypeFailure("(last [])", 0);
        expectArgTypeFailure("(last \"pikachu\")", 0);
    }


    @Test
    public void testFirst()
        throws Exception
    {
        assertEval(0,"(first smList)");
        assertEval(0,"(first medList)");
        assertEval(4000,"(first "+ionListGeneratorWithOffset(1000,4000)+")");
    }

    @Test
    public void testFirstFail()
        throws Exception
    {
        expectArityFailure("(first)");
        expectArityFailure("(first [] 2)");

        expectArgTypeFailure("(first [])", 0);
        expectArgTypeFailure("(first \"boeing\")", 0);
    }


    @Test
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

    @Test
    public void testForListSyntax()
        throws Exception
    {
        expectSyntaxFailure("(for_list)");
        expectSyntaxFailure("(for_list 1 2)");
        expectSyntaxFailure("(for_list null.sexp 13)");
        expectSyntaxFailure("(for_list (12) 13)");
        expectSyntaxFailure("(for_list (1 2) 13)");
        expectSyntaxFailure("(for_list (()) 13)");
        expectSyntaxFailure("(for_list ((12)) 13)");
        expectSyntaxFailure("(for_list ((name)) 13)");
        expectSyntaxFailure("(for_list ((name 1 2)) 13)");
        expectSyntaxFailure("(for_list ((name 1) ()) 13)");
        expectSyntaxFailure("(for_list ((name 1) (name2)) 13)");
    }


    //========================================================================
    // Concatenation

    /** Tests interaction with IonValues. */
    @Test
    public void testConcatenation()
        throws Exception
    {
        Object vector = eval("(stretchy_vector 1)");
        IonList list = (IonList) system().singleValue("[2, sym, true]");
        Object result = topLevel().call("concatenate_m", vector, list);
        assertSame(vector, result);
        assertEquals(4, unsafeVectorSize(null, result));

        // Elements should be lazily fusion-ized
        assertSame(list.get(1), topLevel().call("vector_ref", result, 2));

        // Now mutate the IonValue
        vector = eval("(stretchy_vector 4)");
        result = topLevel().call("concatenate_m", list, vector);
        assertEquals(4, unsafeVectorSize(null, result));

        assertSame(list.get(0), unsafeVectorRef(null, result, 0));
        assertSame(list.get(1), unsafeVectorRef(null, result, 1));
        assertSame(list.get(2), unsafeVectorRef(null, result, 2));
        assertSame(unsafeVectorRef(null, vector, 0),
                   unsafeVectorRef(null, result, 3));
    }


    //========================================================================
    // List iteration

    @Test
    public void testInvalidIteration()
        throws Exception
    {
        expectContractFailure("(list_iterator 3)");
        expectContractFailure("(list_from_iterator 3)");
        expectContractFailure("(list_from_iterator [2,3])");
    }


    @Test()
    public void testIterationArityCheck()
        throws Exception
    {
        expectArityFailure("(list_iterator)");
        expectArityFailure("(list_iterator [] 1)");

        expectArityFailure("(list_from_iterator)");
        expectArityFailure("(list_from_iterator empty_iterator 1)");
    }
}
