// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import java.io.InputStream;
import java.util.Iterator;
import org.junit.Test;


public class LiteralEvalTest
    extends CoreTestCase
{
    @Test
    public void testSelfEval()
        throws Exception
    {
        InputStream data = getClass().getResourceAsStream("selfeval.ion");
        Iterator<IonValue> clauses = system().iterate(data);
        do {
            IonValue expr = clauses.next();
            assertEval(expr, expr);
        }
        while (clauses.hasNext());
    }


    @Test(expected = SyntaxFailure.class)
    public void testNullSymbol()
        throws Exception
    {
        eval("null.symbol");
    }


    @Test(expected = SyntaxFailure.class)
    public void testNullSexp()
        throws Exception
    {
        eval("null.sexp");
    }


    @Test(expected = SyntaxFailure.class)
    public void testEmptySexp()
        throws Exception
    {
        eval("()");
    }


    @Test
    public void testList()
        throws Exception
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
        throws Exception
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

    @Test
    public void testQuote()
        throws Exception
    {
        eval("(define x 13)");
        assertEval("x", "(quote x)");
        assertEval("{f:x,g:true}", "(quote {g:true, f:x})");
        assertEval("[x,99,[]]", "(quote [x, 99, [ ] ])");
    }

    @Test
    public void testUndef()
        throws Exception
    {
        assertEval(true,  "(is_undef undef)");
        assertEval(false, "(is_undef null)");
        assertEval(false, "(is_undef true)");
        assertEval(false, "(is_undef false)");
        assertEval(false, "(is_undef null.bool)");
        assertEval(false, "(is_undef 0)");
        assertEval(false, "(is_undef (quote f))");
        assertEval(false, "(is_undef \"\")");
        assertEval(false, "(is_undef [])");
        assertEval(false, "(is_undef (quote ()))");
        assertEval(false, "(is_undef {})");
    }

    @Test
    public void testIsUndefArity()
        throws Exception
    {
        expectArityFailure("(is_undef)");
        expectArityFailure("(is_undef undef 2)");
    }

    @Test
    public void testIsUndefLeniency()
        throws Exception
    {
        for (String e : allTypeExpressions())
        {
            eval("(is_undef " + e + ")");
        }
    }


    @Test
    public void testIsNull()
        throws Exception
    {
        assertEval(true,  "(is_null null)");
        assertEval(true,  "(is_null null.int)");
        assertEval(false, "(is_null undef)");
        assertEval(false, "(is_null false)");
        assertEval(false, "(is_null 0)");
        assertEval(false, "(is_null \"\")");
        assertEval(false, "(is_null [])");
        assertEval(false, "(is_null (quote ()))");
        assertEval(false, "(is_null {{}})");
    }

    @Test
    public void testIsNullArity()
        throws Exception
    {
        expectArityFailure("(is_null)");
        expectArityFailure("(is_null null 2)");
    }

    @Test
    public void testIsNullLeniency()
        throws Exception
    {
        for (String e : allTypeExpressions())
        {
            eval("(is_null " + e + ")");
        }
    }
}
