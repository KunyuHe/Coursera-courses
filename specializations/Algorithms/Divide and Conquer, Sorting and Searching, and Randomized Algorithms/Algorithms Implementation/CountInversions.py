def countInversions(arr):
    # Base case
    if len(arr) <= 1:
        return arr, 0

    cut = len(arr) // 2
    left, right = arr[:cut], arr[cut:]

    # Count inversions within left and right subarrays
    left_sorted, left_inversions = countInversions(left)
    right_sorted, right_inversions = countInversions(right)

    i, j = 0, 0
    arr_sorted = []
    cnt = left_inversions + right_inversions
    # Merge sort, increase the number of split sort when any element on the
    # right is added to the overall sorted, by the number of elements left
    # in the sorted left array
    while i < len(left_sorted) and j < len(right_sorted):
        if right_sorted[j] < left_sorted[i]:
            arr_sorted.append(right_sorted[j])
            cnt += len(left_sorted) - i
            j += 1
        else:
            arr_sorted.append(left_sorted[i])
            i += 1

    arr_sorted = arr_sorted + left_sorted[i:] + right_sorted[j:]

    return arr_sorted, cnt
