# Training data: 15,000 pairs of the same character type, 15,000 pairs of
# different character types, constructed from the first 12 drawers of the first
# 15 characters of each of 20 randomly selected alphabets.
#
# Test data: 5,000 pairs of the same character type, 5,000 pairs of
# different character types, constructed from all 20 drawers
# of the first 15 characters of each of 10 randomly selected alphabets that were
# different from the alphabets chosen for training.
#
# The same pairs were sampled uniformly over characters within each language,
# such that an equal number of samples were taken from each language, though the
# number of occurrences of each character was not necessarily the same.
#
# The different pairs were sampled uniformly over all combinations of characters
# across the languages, so each language is not necessarily represented equally.
#
# In both cases, sampling is done in a manner that ensures that no pair of
# tokens occurs more than once, i.e. no pair is repeated.


import numpy as np
import itertools as it
import csv, os
from os.path import basename, normpath
from scipy.special import comb


def get_files(dir, inds, fullpath=False):
    """
    Get a slice of files from a directory.
    :param dir: the directory in which to look for files.
    :param inds: file indices, an iterable with the start index and the end
    index in the first and second position.
    :param fullpath: logical, whether to return the full paths (relative to
    the working directory) or just the filenames.
    :return: list of files between the first and second index, where the order
    of files is determined by the list.sort() method.
    """
    if fullpath:
        out = listdir_fullpath(dir)
    else:
        out = os.listdir(dir)
    out.sort()

    return out[inds[0]:inds[1]]


def listdir_fullpath(d):
    """
    List files in a directory with full paths (relative to the working directory)
    """
    return [os.path.join(d, f) for f in os.listdir(d)]


def multinomial_robust(n, ncats, threshold, max_tries=100):
    """
    Generate a multinomial sample with no element larger than some threshold.
    :param n: int
    Number of experiments.
    :param ncats: Number of categories. Sampling is uniform over these
    categories.
    :param threshold: Threshold: if any value in the sample is larger than this
    threshold, then a new sample is generated.
    :param max_tries: The number of times to try to generate a sample whose
    elements are <= the threshold.
    :return: A vector of counts, if successful. If not, an exception is raised.
    """
    okay_flag = False
    tries = 0
    out = None
    while not okay_flag and tries < max_tries:
        tries += 1
        out = np.random.multinomial(n, pvals=np.full(ncats, 1/ncats))
        if all(out <= threshold):
            okay_flag = True

    if tries == max_tries:
        raise Exception('Failed to generate a multinomial sample with all'
                        'values below the threshold in %d tries' %max_tries)

    return out


def sample_different(lang_dirs, char_inds, drawer_inds, n, verbose=True):
    """
    Sample pairs of different characters.
    :param lang_dirs: Language directories, which should contain character
    directories, which should in turn contain the images for each character.
    :param char_inds: An iterable containing the indices of characters to
    include in the sampling, e.g. (0, 15) to include the first 15 characters,
    where the order is determined by the list.sort() method.
    :param drawer_inds: An iterable containing the indices of drawers to include
    in the sampling, e.g. (12, 16) to include drawers 13-16.
    :param n: The total number of samples to draw. Samples are drawn uniformly
    over all combinations of characters from across all the languages, then
    uniformly over all combinations of drawers within a particular pair.
    :param verbose: Logical. If True, just prints a message about the sampling.
    :return: An n-by-8 Numpy array with one row per sample. Each row contains
    the full (relative) filepaths of the two characters in the sample, followed
    by the two source languages for the sample, followed by the two character
    ids, followed by the two filenames (without the full paths).
    """
    if verbose:
        print("Sampling pairs from various languages...")
    out = np.empty((n, 8), np.object_)
    max_samples_per_pair = (drawer_inds[1] - drawer_inds[0]) ** 2
    i = 0

    char_list = [get_files(ll, char_inds, True) for ll in lang_dirs]  # get all chars
    char_list = list(it.chain.from_iterable(char_list))               # flatten list
    char_pairs = list(it.combinations(char_list, 2))                  # get all pairs
    char_counts = multinomial_robust(n, len(char_pairs), max_samples_per_pair)

    # for each character pair, choose token pairs
    i = 0
    for index, count in filter(lambda x: x[1] > 0, enumerate(char_counts)):
        charpath1, charpath2 = char_pairs[index]
        lang1, char1 = normpath(charpath1).split('/')[-2:]
        lang2, char2 = normpath(charpath2).split('/')[-2:]

        token_list1 = get_files(charpath1, drawer_inds)
        token_list2 = get_files(charpath2, drawer_inds)
        pairs_list = np.array(list(it.product(token_list1, token_list2)))
        token_pairs = pairs_list[np.random.choice(len(pairs_list), count, replace=False)]

        for pair in token_pairs:
            newrow = [os.path.join(charpath1, pair[0]),
                      os.path.join(charpath2, pair[1]),
                      lang1, lang2, char1, char2, pair[0], pair[1]]
            out[i] = newrow
            i += 1

    return out


def sample_same(lang_dirs, char_inds, drawer_inds, n, verbose=True):
    """
    Sample pairs of same characters.
    :param lang_dirs: Language directories, which should contain character
    directories, which should in turn contain the images for each character.
    :param char_inds: An iterable containing the indices of characters to
    include in the sampling, e.g. (0, 15) to include the first 15 characters,
    where the order is determined by the list.sort() method.
    :param drawer_inds: An iterable containing the indices of drawers to include
    in the sampling, e.g. (12, 16) to include drawers 13-16.
    :param n: The total number of samples to draw. An equal number of samples
    will be drawn from each language, rounded down. Within a language, samples
    are drawn uniformly over characters, then uniformly over combinations of
    drawers within a given character.
    :param verbose: Logical. If True, prints a message for each language.
    :return: An n-by-6 Numpy array with one row per sample. Each row contains
    the full (relative) filepaths of the two characters in the sample, followed
    by the two source languages for the sample, followed by the two character
    ids, followed by the two filenames (without the full paths).
    """
    out = np.empty((n,6), np.object_)
    n_per_lang = np.floor(n/len(lang_dirs))
    max_samples_per_char = comb(drawer_inds[1] - drawer_inds[0], 2)
    i = 0

    for ll in lang_dirs:
        language = basename(normpath(ll))
        if verbose:
            print("Sampling same pairs from %s..." %language)
        char_list = get_files(ll, char_inds)
        char_counts = multinomial_robust(n_per_lang, len(char_list), max_samples_per_char)

        # for each character, choose token pairs
        for index, count in filter(lambda x: x[1] > 0, enumerate(char_counts)):
            char = char_list[index]
            token_list = get_files(os.path.join(ll, char), drawer_inds)
            pairs_list = np.array(list(it.combinations(token_list, 2)))
            char_pairs = pairs_list[np.random.choice(len(pairs_list), count, replace=False)]
            for pair in char_pairs:
                newrow = [os.path.join(ll, char, pair[0]),
                          os.path.join(ll, char, pair[1]),
                          language, char, pair[0], pair[1]]
                out[i] = newrow
                i += 1

    return out


def split_langs(data_dir, ntrain=20, ntest=10):
    """
    Select test and training languages.
    :param data_dir: Directory containing the language directories.
    :param ntrain: The number of training languages.
    :param ntest: The number of test languages.
    :return: A tuple of two lists of language names: the training languages,
    then the test langauges.
    """
    language_list = listdir_fullpath(data_dir)
    np.random.shuffle(language_list)
    train = language_list[0:ntrain]
    test = language_list[ntrain:(ntrain + ntest)]

    return train, test


if __name__ == "__main__":
    data_dir = "../images/omniglot/images_background/"
    train_langs, test_langs = split_langs(data_dir, 20, 10)

    # Create training pairs
    print("Creating training samples: same pairs")
    train_same = sample_same(train_langs, (0, 15), (0, 12), 15000)
    with open("../images/omniglot/train_same_pairs.csv", 'w') as f:
        csv.writer(f).writerows(train_same)

    print("Creating training samples: different pairs")
    train_diff = sample_different(train_langs, (0, 15), (0, 12), 15000)
    with open("../images/omniglot/train_different_pairs.csv", 'w') as f:
        csv.writer(f).writerows(train_diff)

    # Create test pairs
    print("Creating test samples: same pairs")
    test_same = sample_same(test_langs, (0, 15), (0, 20), 5000)
    with open("../images/omniglot/test_same_pairs.csv", 'w') as f:
        csv.writer(f).writerows(test_same)

    print("Creating test samples: different pairs")
    test_diff = sample_different(test_langs, (0, 15), (0, 20), 5000)
    with open("../images/omniglot/test_different_pairs.csv", 'w') as f:
        csv.writer(f).writerows(test_diff)