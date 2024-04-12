% dataset(DirectoryName) 
% this is where the image dataset is located
dataset('C:\\Users\\Documents\\imageDataset2_15_20\\').
% directory_textfiles(DirectoryName, ListOfTextfiles)
% produces the list of text files in a directory
directory_textfiles(D,Textfiles):- directory_files(D,Files), include(isTextFile, Files, Textfiles).
isTextFile(Filename):-string_concat(_,'.txt',Filename).
% read_hist_file(Filename,ListOfNumbers)
% reads a histogram file and produces a list of numbers (bin values)
read_hist_file(Filename,Numbers):- open(Filename,read,Stream),read_line_to_string(Stream,_),
                                   read_line_to_string(Stream,String), close(Stream),
								   atomic_list_concat(List, ' ', String),atoms_numbers(List,Numbers).
								   
% similarity_search(QueryFile,SimilarImageList)
% returns the list of images similar to the query image
% similar images are specified as (ImageName, SimilarityScore)
% predicat dataset/1 provides the location of the image set
similarity_search(QueryFile,SimilarList) :- dataset(D), directory_textfiles(D,TxtFiles),
                                            similarity_search(QueryFile,D,TxtFiles,SimilarList).
											
% similarity_search(QueryFile, DatasetDirectory, HistoFileList, SimilarImageList)
similarity_search(QueryFile,DatasetDirectory, DatasetFiles,Best):- read_hist_file(QueryFile,QueryHisto), 
                                            compare_histograms(QueryHisto, DatasetDirectory, DatasetFiles, Scores), 
                                            sort(2,@>,Scores,Sorted),take(Sorted,5,Best).

% compare_histograms(QueryHisto,DatasetDirectory,DatasetFiles,Scores)
% compares a query histogram with a list of histogram files 
compare_histograms(_, _, [], []). % Base case: Empty list of files
compare_histograms(QueryHisto, DatasetDirectory, [File|RestFiles], [(File, Score)|RestScores]) :-
    atom_concat(DatasetDirectory, File, FilePath),
    read_hist_file(FilePath, FileHisto),
    histogram_intersection(QueryHisto, FileHisto, Score),
    compare_histograms(QueryHisto, DatasetDirectory, RestFiles, RestScores).

% histogram_intersection(Histogram1, Histogram2, Score)
% compute the intersection similarity score between two histograms
% Score is between 0.0 and 1.0 (1.0 for identical histograms)
% histogram_intersection(H1,H2,S):- ... ?
histogram_intersection([], [], 1.0). % If both histograms are empty, they are identical
histogram_intersection([H1|T1], [H2|T2], Score) :-
    histogram_intersection(T1, T2, RestScore), % Recursively calculate the score for the rest of the histograms
    MinValue is min(H1, H2), % Calculate the minimum value of the corresponding bins
    Score is RestScore + MinValue. % Accumulate the minimum values

% Normalize the score based on the total count of bins
normalize_score(_, 0, 0.0). % Avoid division by zero
normalize_score(Score, TotalBins, NormalizedScore) :-
    NormalizedScore is Score / TotalBins.

% take(List,K,KList)
% extracts the K first items in a list
take(Src,N,L) :- findall(E, (nth1(I,Src,E), I =< N), L).
% atoms_numbers(ListOfAtoms,ListOfNumbers)
% converts a list of atoms into a list of numbers
atoms_numbers([],[]).
atoms_numbers([X|L],[Y|T]):- atom_number(X,Y), atoms_numbers(L,T).
atoms_numbers([X|L],T):- \+atom_number(X,_), atoms_numbers(L,T).