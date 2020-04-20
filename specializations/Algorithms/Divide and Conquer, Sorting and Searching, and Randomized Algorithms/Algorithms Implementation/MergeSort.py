def split(arr):
    cut = len(arr) // 2
    return arr[:cut], arr[cut:]


def sort(left, right):
    # If any of the two is empty, return the other
    if len(left) == 0:
        return right
    elif len(right) == 0:
        return left

    # Fill the output array with the merged lists
    i, j = 0, 0
    output = []

    for _ in range(len(left) + len(right)):
        if left[i] <= right[j]:
            output.append(left[i])
            i += 1
        else:
            output.append(right[j])
            j += 1

        # If we are at the end of one, append the rest of the other
        if i == len(left):
            output += right[j:]
            break
        elif j == len(right):
            output += left[i:]
            break
    
    return output


def mergeSort(arr):
    # Base case
    if len(arr) <= 1:
        return arr

    left, right = split(arr)
    return sort(mergeSort(left), mergeSort(right))
