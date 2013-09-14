// Copyright (c) 2005-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

abstract class Command
{
    interface Executor
    {
        /**
         * Execute this command.  Any exception that's thrown will cause the
         * CLI to display the stack trace to {@link System#err}.
         *
         * @return zero to indicate success, any other number to indicate an
         *  error code.
         */
        int execute()
            throws Exception;
    }


    private final String   myCommand;
    private final String[] myAliases;

    private String myHelpOneLiner = null;
    private String myHelpUsage = "your guess is as good as mine!";
    private String myHelpBody = "Sorry, this command is undocumented.\n";


    //=========================================================================
    // Construction and Initialization

    Command(String command, String... aliases)
    {
        myCommand = command;
        myAliases = (aliases == null ? new String[0] : aliases);
    }


    /**
     * Sets the help text for this command.
     *
     * Nomenclature: this is called "put" to hide it from the
     * BeanUtils.copyProperty call in extractOptions
     *
     * @param oneLiner must be non-empty. It should start with a verb and end
     * with a period.
     * @param usage must be non-empty, and must start with the primary command.
     * @param body must be non-empty.  It should not be indented, and it must
     * not end with a newline.  It should be (explicitly) wrapped to display
     * within 80 columns.
     */
    void putHelpText(String oneLiner, String usage, String body)
    {
        assert oneLiner != null && oneLiner.length() > 0;
        assert usage != null && usage.startsWith(myCommand);
        assert body != null && !body.endsWith("\n");

        myHelpOneLiner = oneLiner;
        myHelpUsage    = usage;
        myHelpBody     = body;
    }


    //=========================================================================
    // Property Accessors

    String getCommand()
    {
        return myCommand;
    }

    /**
     * Gets the aliases for this command, generally shortened forms of
     * {@link #getCommand}.
     *
     * @return the array of aliases, not null.
     */
    String[] getAliases()
    {
        return myAliases;
    }

    /**
     * If null, the command will not be listed by `help`.
     */
    String getHelpOneLiner()
    {
        return myHelpOneLiner;
    }

    String getHelpUsage()
    {
        return myHelpUsage;
    }

    String getHelpBody()
    {
        return myHelpBody;
    }


    //=========================================================================
    // Option parsing

    /**
     * Parse and remove command-line options.
     * Options are defined to start with <code>'--'</code>, and come in three
     * forms:
     * <pre>
     *     --key=value
     *     --key value
     *     --key
     *  </pre>
     *  The first two forms are used for all non-boolean option types.
     *  The third form is used for boolean options, which are always set to
     *  <code>true</code> when present on the command line.
     *  <p>
     *  Note that no sanity checking is performed on the value, so if the
     *  user types:
     *  <pre>
     *      --key = value
     *  </pre>
     *   The option <code>"key"</code> will have the value <code>"="</code>.
     */
    String[] extractOptions(String[] args)
    {
        // TODO
        return args;
    }

    //=========================================================================
    // CLI Argument Processing Methods

    /**
     * Verify that the command string used to invoke the command matches
     * the command or one of its aliases.
     */
    boolean matches(String command)
    {
        if (getCommand().equals(command)) return true;
        for (String alias : myAliases)
        {
            if (alias.equals(command)) return true;
        }
        return false;
    }


    /**
     * Perform pre-processing, including in particular argument processing.
     * A null result causes the framework to emit command-specific usage help.
     *
     * @param args to process
     *
     * @return an {@link Executor} to execute the command; null if there are
     * command-line errors preventing the command from being used.
     */
    Executor prepare(String[] args)
    {
        String[] cmdArgs = extractOptions(args);

        return processArguments(cmdArgs);
    }


    /**
     * Parses the command-line arguments to build a {@link Executor} for
     * execution.
     * Note that any options (<em>i.e.</em>, arguments prefixed by
     * {@code "--"}) will have already been extracted from the
     * {@code arguments} array.
     *
     * @param arguments to parse
     * @return null if the arguments are inappropriate or insufficient.
     */
    abstract Executor processArguments(String[] arguments);
}
