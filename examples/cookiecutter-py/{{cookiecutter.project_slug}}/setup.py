from setuptools import setup

setup(name='{{cookiecutter.project_slug}}',
      version='{{cookiecutter.version}}',
      description='{{cookiecutter.description}}',
      url='http://github.com/{{cookiecutter.github_username}}/{{cookiecutter.project_slug}}',
      author='{{cookiecutter.full_name}}',
      author_email='{{cookiecutter.email}}',
      license='{{cookiecutter.license}}',
      packages=['{{cookiecutter.project_slug}}'],
      scripts=['bin/{{cookiecutter.project_slug}}'],
      install_requires=[],
      zip_safe=False)
