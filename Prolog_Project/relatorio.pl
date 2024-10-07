:- module(relatorio, [registrar_acao/1, gerar_relatorio/0, limpar_relatorio/0]).
:- use_module(library(date)).

% Path to the log file
log_file('relatorio.txt').

% Record actions with timestamps in the log file
registrar_acao(Acao) :-
    get_time(TimeStamp),
    stamp_date_time(TimeStamp, DateTime, 'local'),
    format_time(string(TimeString), "%Y-%m-%d %H:%M:%S", DateTime),
    log_file(FilePath),
    open(FilePath, append, Stream),
    format(Stream, '~w - ~w~n', [TimeString, Acao]),
    close(Stream).

% Generate a report by displaying the contents of the log file
gerar_relatorio :-
    log_file(FilePath),
    write('Gerando relatório...'), nl,
    open(FilePath, read, Stream),
    read_file(Stream),
    close(Stream).

% Clear the log file
limpar_relatorio :-
    log_file(FilePath),
    open(FilePath, write, Stream),
    write(Stream, ''),  % Writing empty string to clear the file
    close(Stream),
    write('Relatório limpo.'), nl.

% Helper function to read the log file
read_file(Stream) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    writeln(Line),
    read_file(Stream).
read_file(_).
