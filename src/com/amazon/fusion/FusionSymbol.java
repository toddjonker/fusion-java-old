// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionBool.trueBool;
import static com.amazon.fusion.FusionString.makeString;
import com.amazon.fusion.FusionBool.BaseBool;
import com.amazon.fusion.FusionText.BaseText;
import com.amazon.ion.IonException;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;


final class FusionSymbol
{
    private FusionSymbol() {}


    abstract static class BaseSymbol
        extends BaseText
    {
        private BaseSymbol() {}

        @Override
        BaseSymbol annotate(Evaluator eval, String[] annotations)
        {
            return FusionSymbol.annotate(this, annotations);
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseSymbol)
            {
                String r = ((BaseSymbol) right).stringValue();
                if (r != null)
                {
                    String l = this.stringValue(); // not null
                    if (l.equals(r))
                    {
                        return trueBool(eval);
                    }
                }
            }

            return falseBool(eval);
        }

        @Override
        SyntaxValue wrapAsSyntax(Evaluator eval, SourceLocation loc)
        {
            String value = stringValue();
            if (value != null &&
                value.startsWith("_") &&
                value.endsWith("_"))
            {
                return SyntaxKeyword.make(eval, loc, this);
            }
            return SyntaxSymbol.make(eval, loc, this);
        }

        @Override
        SyntaxValue datumToSyntaxMaybe(Evaluator eval, SourceLocation loc)
            throws FusionException
        {
            return wrapAsSyntax(eval, loc);
        }
    }


    private static class NullSymbol
        extends BaseSymbol
    {
        private NullSymbol() {}

        @Override
        String stringValue()
        {
            return null;
        }

        @Override
        boolean isAnyNull()
        {
            return true;
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            boolean b = (right instanceof BaseSymbol
                         && ((BaseSymbol) right).isAnyNull());
            return makeBool(eval, b);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return isAnyNull(eval, right);
        }

        @Override
        SyntaxValue wrapAsSyntax(Evaluator eval, SourceLocation loc)
        {
            // No need to check for keywords.
            return SyntaxSymbol.make(eval, loc, this);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newNullSymbol();
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeNull(IonType.SYMBOL);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("null.symbol");
        }
    }


    private static class ActualSymbol
        extends BaseSymbol
    {
        private final String myContent;

        private ActualSymbol(String content)
        {
            assert content != null;
            myContent = content;
        }

        @Override
        String stringValue()
        {
            return myContent;
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newSymbol(myContent);
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeSymbol(myContent);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            IonTextUtils.printSymbol(out, myContent);
        }

        @Override
        void display(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append(myContent);
        }
    }


    private static class AnnotatedSymbol
        extends BaseSymbol
        implements Annotated
    {
        /** Not null or empty */
        final String[] myAnnotations;

        /** Not null, and not AnnotatedBool */
        final BaseSymbol  myValue;

        private AnnotatedSymbol(String[] annotations, BaseSymbol value)
        {
            assert annotations.length != 0;
            myAnnotations = annotations;
            myValue = value;
        }

        @Override
        public String[] annotationsAsJavaStrings()
        {
            return myAnnotations;
        }

        @Override
        BaseSymbol annotate(Evaluator eval, String[] annotations)
        {
            return FusionSymbol.annotate(myValue, annotations);
        }

        @Override
        boolean isAnyNull() { return myValue.isAnyNull(); }

        @Override
        String stringValue()
        {
            return myValue.stringValue();
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.tightEquals(eval, right);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.looseEquals(eval, right);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            IonValue iv = myValue.copyToIonValue(factory,
                                                 throwOnConversionFailure);
            iv.setTypeAnnotations(myAnnotations);
            return iv;
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.setTypeAnnotations(myAnnotations);
            myValue.ionize(eval, out);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            writeAnnotations(out, myAnnotations);
            myValue.write(eval, out);
        }
    }


    //========================================================================
    // Constructors


    private static final BaseSymbol NULL_SYMBOL  = new NullSymbol();


    /**
     * @param value must not be empty but may be null to make
     * {@code null.symbol}.
     *
     * @return not null.
     */
    static BaseSymbol makeSymbol(Evaluator eval, String value)
    {
        if (value == null) return NULL_SYMBOL;

        if (value.isEmpty())
        {
            throw new IllegalArgumentException("Cannot make an empty symbol");
        }

        return new ActualSymbol(value);
    }


    private static BaseSymbol annotate(BaseSymbol unannotated,
                                       String[] annotations)
    {
        assert ! (unannotated instanceof AnnotatedSymbol);

        if (annotations.length == 0) return unannotated;

        return new AnnotatedSymbol(annotations, unannotated);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null to make {@code null.symbol}.
     *
     * @return not null.
     */
    static BaseSymbol makeSymbol(Evaluator eval,
                                 String[]  annotations,
                                 String    value)
    {
        BaseSymbol base = makeSymbol(eval, value);
        return annotate(base, annotations);
    }


    /**
     * @param fusionSymbol must be a Fusion symbol.
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseSymbol unsafeSymbolAnnotate(Evaluator eval,
                                           Object fusionSymbol,
                                           String[] annotations)
    {
        BaseSymbol base = (BaseSymbol) fusionSymbol;
        return base.annotate(eval, annotations);
    }


    //========================================================================
    // Predicates


    public static boolean isSymbol(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseSymbol);
    }

    static boolean isSymbol(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseSymbol);
    }


    //========================================================================
    // Conversions


    /**
     * @param fusionSymbol must be a Fusion symbol.
     *
     * @return null if given {@code null.symbol}.
     */
    static String unsafeSymbolToJavaString(Evaluator eval, Object fusionSymbol)
        throws FusionException
    {
        return ((BaseSymbol) fusionSymbol).stringValue();
    }


    /**
     * Converts a Fusion symbol to a {@link String}.
     *
     * @return null if the value isn't a Fusion symbol.
     */
    static String symbolToJavaString(Evaluator eval, Object value)
        throws FusionException
    {
        if (isSymbol(eval, value))
        {
            return unsafeSymbolToJavaString(eval, value);
        }
        return null;
    }


    //========================================================================
    // Procedure Helpers

    /**
     * @param expectation must not be null.
     * @return may be null
     */
    static String checkSymbolArg(Evaluator eval,
                                 Procedure who,
                                 String    expectation,
                                 int       argNum,
                                 Object... args)
        throws FusionException, ArgumentException
    {
        Object arg = args[argNum];
        if (arg instanceof BaseSymbol)
        {
            return ((BaseSymbol) arg).stringValue();
        }

        throw who.argFailure(expectation, argNum, args);
    }


    /**
     * @return may be null
     */
    static String checkNullableSymbolArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgumentException
    {
        String expectation = "nullable symbol";
        return checkSymbolArg(eval, who, expectation, argNum, args);
    }


    /**
     * @return not null
     */
    static String checkRequiredSymbolArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgumentException
    {
        String expectation = "non-null symbol";
        String result = checkSymbolArg(eval, who, expectation, argNum, args);
        if (result == null)
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    /**
     * @deprecated Use
     * {@link #checkNullableSymbolArg(Evaluator, Procedure, int, Object...)}.
     */
    @Deprecated
    static String checkNullableArg(Procedure who, int argNum, Object... args)
        throws FusionException, ArgumentException
    {
        return checkNullableSymbolArg(null, who, argNum, args);
    }

    /**
     * @deprecated Use
     * {@link #checkRequiredSymbolArg(Evaluator, Procedure, int, Object...)}.
     */
    @Deprecated
    static String checkRequiredArg(Procedure who, int argNum, Object... args)
        throws FusionException, ArgumentException
    {
        return checkRequiredSymbolArg(null, who, argNum, args);
    }



    //========================================================================
    // Procedures


    static final class IsSymbolProc
        extends Procedure1
    {
        IsSymbolProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is of type `symbol`, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean r = isSymbol(eval, arg);
            return makeBool(eval, r);
        }
    }


    static final class ToStringProc
        extends Procedure
    {
        ToStringProc()
        {
            //    "                                                                               |
            super("Converts a `symbol` to a string with the same text. Returns `null.string` when\n"
                + "given `null.symbol`.",
                  "symbol");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkNullableArg(this, 0, args);
            return makeString(eval, input);
        }
    }
}
