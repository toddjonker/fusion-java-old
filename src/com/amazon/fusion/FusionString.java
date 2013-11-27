// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionText.checkRequiredTextArg;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionUtils.safeEquals;
import com.amazon.ion.IonException;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.util.Arrays;


final class FusionString
{
    private FusionString() {}


    abstract static class BaseString
        extends FusionValue
    {
        private BaseString() {}

        String[] annotationsAsJavaStrings()
        {
            return EMPTY_STRING_ARRAY;
        }

        abstract String stringValue();

        @Override
        public boolean equals(Object o)
        {
            if (this == o) return true;
            if (o instanceof BaseString)
            {
                BaseString that = (BaseString) o;
                return (safeEquals(this.stringValue(), that.stringValue())
                        && Arrays.equals(this.annotationsAsJavaStrings(),
                                         that.annotationsAsJavaStrings()));
            }
            return false;
        }
    }


    private static class NullString
        extends BaseString
    {
        private NullString() {}

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
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newNullString();
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeNull(IonType.STRING);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("null.string");
        }
    }


    private static class ActualString
        extends BaseString
    {
        private final String myContent;

        private ActualString(String content)
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
            return factory.newString(myContent);
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeString(myContent);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            IonTextUtils.printString(out, myContent);
        }

        @Override
        void display(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append(myContent);
        }
    }


    private static class AnnotatedString
        extends BaseString
        implements Annotated
    {
        /** Not null or empty */
        final String[] myAnnotations;

        /** Not null, and not AnnotatedBool */
        final BaseString  myValue;

        private AnnotatedString(String[] annotations, BaseString value)
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
        boolean isAnyNull() { return myValue.isAnyNull(); }

        @Override
        String stringValue()
        {
            return myValue.stringValue();
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


    private static final BaseString NULL_STRING  = new NullString();


    /**
     * @param value may be null.
     *
     * @return not null.
     */
    static BaseString makeString(Evaluator eval, String value)
    {
        return (value == null ? NULL_STRING : new ActualString(value));
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null to make {@code null.string}.
     *
     * @return not null.
     */
    static BaseString makeString(Evaluator eval,
                                 String[]  annotations,
                                 String    value)
    {
        BaseString base = makeString(eval, value);

        if (annotations.length != 0)
        {
            base = new AnnotatedString(annotations, base);
        }

        return base;
    }


    /**
     * @param fusionString must be a Fusion string.
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseString unsafeStringAnnotate(Evaluator eval,
                                           Object fusionString,
                                           String[] annotations)
    {
        BaseString base = (BaseString) fusionString;
        if (base instanceof AnnotatedString)
        {
            base = ((AnnotatedString) base).myValue;
        }
        return new AnnotatedString(annotations, base);
    }


    //========================================================================
    // Predicates


    public static boolean isString(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseString);
    }

    static boolean isString(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseString);
    }


    //========================================================================
    // Conversions


    /**
     * @param value must be a Fusion string.
     *
     * @return null if given {@code null.string}.
     */
    static String unsafeStringToJavaString(Evaluator eval, Object value)
        throws FusionException
    {
        return ((BaseString) value).stringValue();
    }


    /**
     * Converts a Fusion string to a {@link String}.
     *
     * @return null if the value isn't a Fusion string.
     */
    static String stringToJavaString(Evaluator eval, Object value)
        throws FusionException
    {
        if (isString(eval, value))
        {
            return unsafeStringToJavaString(eval, value);
        }
        return null;
    }


    //========================================================================
    // Procedure Helpers


    /**
     * @param expectation must not be null.
     * @return may be null
     */
    static String checkStringArg(Evaluator eval,
                                 Procedure who,
                                 String    expectation,
                                 int       argNum,
                                 Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof BaseString)
        {
            return ((BaseString) arg).stringValue();
        }

        throw who.argFailure(expectation, argNum, args);
    }


    /**
     * @return may be null
     */
    static String checkNullableStringArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "nullable string";
        return checkStringArg(eval, who, expectation, argNum, args);
    }


    /**
     * @return not null
     */
    static String checkRequiredStringArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-null string";
        String result = checkStringArg(eval, who, expectation, argNum, args);
        if (result == null)
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    /**
     * @return not null or empty
     */
    static String checkNonEmptyStringArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-empty string";
        String result = checkStringArg(eval, who, expectation, argNum, args);
        if (result == null || result.isEmpty())
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    //========================================================================
    // Procedures


    static final class IsStringProc
        extends Procedure1
    {
        IsStringProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is of type `string`, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean r = isString(eval, arg);
            return makeBool(eval, r);
        }
    }


    static final class AppendProc
        extends Procedure
    {
        AppendProc()
        {
            //    "                                                                               |
            super("Concatenates the `text` values (strings or symbols), returning a string.  If no\n" +
                  "arguments are supplied, the result is `\"\"`.",
                  "text", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            StringBuilder resultBuilder = new StringBuilder();

            for (int i = 0; i < args.length; i++)
            {
                String v = checkRequiredTextArg(eval, this, i, args);
                resultBuilder.append(v);
            }

            return eval.newString(resultBuilder.toString());
        }
    }



    static final class ToLowerProc
        extends Procedure
    {
        ToLowerProc()
        {
            super("Converts all the characters in a `string` to lower-case letters.",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkRequiredStringArg(eval, this, 0, args);
            return eval.newString(input.toLowerCase());
        }
    }



    static final class ToUpperProc
        extends Procedure
    {
        ToUpperProc()
        {
            super("Converts all the characters in a `string` to upper-case letters.",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkRequiredStringArg(eval, this, 0, args);
            return eval.newString(input.toUpperCase());
        }
    }



    static final class ToSymbolProc
        extends Procedure
    {
        ToSymbolProc()
        {
            //    "                                                                               |
            super("Converts a `string` to a symbol with the same text.  Returns `null.symbol` when\n"
                + "given `null.string`.  Raises an exception when given an empty string.",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkNullableStringArg(eval, this, 0, args);

            if (input != null && input.isEmpty())
            {
                throw argFailure("non-empty string", 0, args);
            }

            return eval.newSymbol(input);
        }
    }
}
