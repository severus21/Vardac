def normalize_path(path, sep="-"):
    for c in ' [](),':
        path = path.replace(c, sep)
    return path