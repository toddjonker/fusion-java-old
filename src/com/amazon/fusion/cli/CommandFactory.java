// Copyright (c) 2005-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.cli.Command.Executor;

class CommandFactory
{
    public static final String APP_NAME = "fusion";

    private static final int USAGE_ERROR_CODE = 1;


    public static Command[] getAllCommands()
    {
        Command[] commands =
            new Command[]
            {
                new Repl(),
                new Load(),
                new Cover(),
                new Separator(),
                new Help(),
                new Version(),
                new Document(),
            };

        return commands;
    }


    public static Command getMatchingCommand(String subcommand)
    {
        Command[] allCommands = getAllCommands();

        for (Command cmd : allCommands)
        {
            if (cmd.matches(subcommand))
            {
                return cmd;
            }
        }

        return null;
    }


    private static void writeUsage(Command cmd)
    {
        if (cmd != null)
        {
            System.err.print("Usage: ");
            System.err.println(cmd.getHelpUsage());
        }

        System.err.print("Type '" + APP_NAME + " help");

        if (cmd != null)
        {
            System.err.print(' ');
            System.err.print(cmd.getCommand());
        }

        System.err.println("' for more information.");
    }


    /**
     *
     * @param commandLine must not be <code>null</code>.
     *
     * @return the {@link Command} to execute; not null.
     *
     * @throws UsageException if there are
     * command-line errors preventing the command from being used.     */
    public static Command matchCommand(GlobalOptions globals,
                                       String[] commandLine)
        throws Exception
    {
        if (commandLine.length == 0)
        {
            throw new UsageException("No command given.");
        }

        String command = commandLine[0];

        Command cmd = getMatchingCommand(command);
        if (cmd == null)
        {
            throw new UsageException("Unknown command: '" + command + "'");
        }
        return cmd;
    }


    /**
     * @param commandLine includes the leading command name.
     *
     * @return an {@link Executor} to execute the command; not null.
     *
     * @throws UsageException if there are
     * command-line errors preventing the command from being used.
     */
    public static Executor makeExecutor(GlobalOptions globals,
                                        Command  command,
                                        String[] commandLine)
        throws Exception
    {
        // Strip off the leading command name, leaving the options and argse
        int argCount = commandLine.length - 1;
        String[] args = new String[argCount];
        System.arraycopy(commandLine, 1, args, 0, argCount);

        Executor exec = command.prepare(globals, args);
        if (exec == null)
        {
            throw new UsageException(command, null);
        }
        return exec;
    }


    /**
     * @return an error code, zero meaning success.
     */
    public static int executeCommandLine(String[] commandLine)
        throws Exception
    {
        int errorCode = 0;

        GlobalOptions globals = new GlobalOptions();

        try
        {
            commandLine = Command.extractOptions(globals, commandLine, true);

            int curStartPos = 0;
            for (int i = 0; i < commandLine.length && errorCode == 0; i++)
            {
                if (";".equals(commandLine[i])) {
                    int len = i-curStartPos;
                    if (len > 0) {
                        errorCode = doExecutePartial(globals,
                                                     commandLine,
                                                     curStartPos,
                                                     len);
                    }
                    curStartPos = i+1;
                }
            }
            int len = commandLine.length-curStartPos;
            if (errorCode == 0) {
                errorCode = doExecutePartial(globals, commandLine, curStartPos, len);
            }

            return errorCode;
        }
        catch (UsageException e)
        {
            System.err.println();
            String message = e.getMessage();
            if (message != null)
            {
                System.err.println(message);
                System.err.println();
            }
            writeUsage(e.myCommand);
            return USAGE_ERROR_CODE;
        }
    }


    /**
     * @return an error code, zero meaning success.
     */
    private static int doExecutePartial(GlobalOptions globals,
                                        String[] commandLine,
                                        int start,
                                        int len)
        throws Exception
    {
        String[] partial = new String[len];
        System.arraycopy(commandLine, start, partial, 0, len);

        Command command = matchCommand(globals, partial);

        Executor exec = makeExecutor(globals, command, partial);

        return exec.execute();
    }

    static class Separator extends Command
    {

        Separator() {
            super("-----");
            putHelpText("-------------------------", "-----", "-----");
        }

        @Override
        boolean matches(String command)
        {
            return false;
        }

        @Override
        public Executor makeExecutor(String[] arguments)
        {
            return null;
        }
    }
}
