-module(afile_client).
-export([ls/1, get_file/2]).

ls(FileServer) ->
    FileServer ! {self(), list_dir},
    receive
        {FileServer, FileList} ->
            FileList
    end.

get_file(FileServer, File) ->
    FileServer ! {self(), {get_file, File}},
    receive
        {FileServer, Content} ->
            Content
    end.
