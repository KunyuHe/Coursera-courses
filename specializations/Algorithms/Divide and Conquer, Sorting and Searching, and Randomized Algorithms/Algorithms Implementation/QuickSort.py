CNT = 0


def getPivot(arr, low, high, rule):
    if rule == "First":
        return low
    elif rule == "Last":
        return high - 1
    elif rule == "Median":
        median = sorted([(low, arr[low]),
                         (high-1, arr[high-1]),
                         (low + (high-low-1)//2, arr[low + (high-low-1)//2])],
                        key=lambda x: x[1])[1][0]
        return median


def partition(arr, low, high, p):
    # Use the first element as pivot
    if p != low:
        arr[p], arr[low] = arr[low], arr[p]
    pivot = arr[low]

    i = low + 1
    for j in range(low + 1, high):
        # If current element is smaller than or equal to pivot, swap
        if arr[j] <= pivot:
            arr[i], arr[j] = arr[j], arr[i]
            # Increment the index dividing elements smaller than (or equal to)
            # the pivot and those larger than the pivot
            i += 1

    arr[i-1], arr[low] = arr[low], arr[i-1]
    return i - 1


def quickSort(arr, low, high, rule):
    if low < high - 1:
        global CNT
        # pi is partitioning index, arr[pi] is now at right place
        pi = partition(arr, low, high, getPivot(arr, low, high, rule))
        CNT += high - low - 1
        # Separately sort elements before partition and after partition
        quickSort(arr, low, pi, rule)
        quickSort(arr, pi + 1, high, rule)

def countComparisons(arr, rule="First"):
    global CNT
    CNT = 0
    quickSort(arr, 0, len(arr), rule)
    return CNT
