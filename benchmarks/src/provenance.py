import git

repo = git.Repo(search_parent_directories=True)

def get_current_stamp():
    sha = repo.head.object.hexsha
    return sha