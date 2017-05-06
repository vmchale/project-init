from setuptools import setup

setup(name='{{ project }}',
      version='{{ version }}',
      description='{{ description }}',
      url='http://github.com/{{ github_username}}/{{ project }}',
      author='{{ name }}',
      author_email='{{ email }}',
      license='{{ license }}',
      packages=['{{ project }}'],
      scripts=['bin/{{ project }}'],
      install_requires=[],
      zip_safe=False)
